package calculation
import calculation.Rational.RationalExpr

object ExpressionTree {
  trait Expr {
    def getValueOption(context: Map[String, ValueType]=Map.empty): Option[ValueType] = this match {
      case x: InstantExpr => Some(x.value)
      case x: FreeExpr => x computeValue context
    }

    def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] = this match {
      case x: InstantExpr => Some(x.value)
      case x: FreeExpr => x.computeExpression(context)
    }

    def collectFreeVariable: Set[String]
  }

  trait InstantExpr extends Expr {
    def value: ValueType = getValueOption(Map.empty).get

    override def collectFreeVariable: Set[String] = Set.empty
  }

  trait FreeExpr extends Expr {
    def computeValue(context: Map[String, ValueType]): Option[ValueType]
    def computeExpression(context: Map[String, Expr]): Option[ValueType]
  }

  case class BinaryOperatorTree(lch: Expr, rch: Expr, op: (ValueType, ValueType) => ValueType) extends InstantExpr {
    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      lch.getValueOption(context).flatMap(x => rch.getValueOption(context).map(op(x, _)))

    override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
      lch.computeExpressionOption(context).flatMap(x => rch.computeExpressionOption(context).map(op(x, _)))

    override def collectFreeVariable: Set[String] = lch.collectFreeVariable ++ rch.collectFreeVariable
  }

  case class ValueLeaf(override val value: ValueType) extends InstantExpr

  case class SingleOperatorTree(node: Expr, op: ValueType => ValueType) extends InstantExpr {
    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      node.getValueOption(context).map(op)

    override def collectFreeVariable: Set[String] = node.collectFreeVariable
  }

  case class FreeVariableLeaf(name: String) extends FreeExpr {
    override def computeValue(context: Map[String, ValueType]): Option[ValueType] =
      context.get(name)

    override def computeExpression(context: Map[String, Expr]): Option[ValueType] =
      context.get(name) flatMap (_.computeExpressionOption(context))

    override def collectFreeVariable: Set[String] = Set(name)
  }

  implicit def fromValue(value: ValueType): ValueLeaf = ValueLeaf(value)
  implicit def fromInteger(value: Int): ValueLeaf = RationalExpr(value)
  implicit def fromDouble(value: Double): ValueLeaf = DoubleValue(value)

}
