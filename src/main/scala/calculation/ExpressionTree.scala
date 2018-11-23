package calculation
import calculation.Rational.{Rational, RationalExpr}

import scala.language.implicitConversions

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

    def replaceByContext(context: Map[String, Expr]): Expr = this

    def flatten: Expr = flattenOption getOrElse this

    def flattenOption: Option[ValueLeaf] = getValueOption() map ValueLeaf
  }

  trait InstantExpr extends Expr {
    def value: ValueType = getValueOption(Map.empty).get

    override def collectFreeVariable: Set[String] = Set.empty
  }

  case class PartialAppliedExpression(expr: Expr, context: Map[String, Expr]) extends Expr {
    override def collectFreeVariable: Set[String] = expr.collectFreeVariable -- context.keySet

    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      this.expr.computeExpressionOption(context.mapValues(ValueLeaf) ++ this.context)

    override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
      this.expr.computeExpressionOption(context ++ this.context)

    override def replaceByContext(context: Map[String, Expr]): Expr = expr.replaceByContext(context).replaceByContext(this.context)

    override def toString: String = s"(${expr.toString}) where (${context.mkString(",")})"
  }

  trait FreeExpr extends Expr {
    def computeValue(context: Map[String, ValueType]): Option[ValueType]
    def computeExpression(context: Map[String, Expr]): Option[ValueType]
  }

  trait WithOperatorName {
    var operatorName: String = ""
    def displayName:String = if (operatorName.length != 0) operatorName else "$lambda"
    def withName(name: String):this.type = {
      operatorName = name
      this
    }
  }

  case class BinaryOperatorTree(lch: Expr, rch: Expr, op: (ValueType, ValueType) => ValueType) extends InstantExpr with WithOperatorName {
    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      lch.getValueOption(context).flatMap(x => rch.getValueOption(context).map(op(x, _)))

    override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
      lch.computeExpressionOption(context).flatMap(x => rch.computeExpressionOption(context).map(op(x, _)))

    override def collectFreeVariable: Set[String] = lch.collectFreeVariable ++ rch.collectFreeVariable

    override def replaceByContext(context: Map[String, Expr]): Expr =
      copy(lch = lch replaceByContext context, rch = rch replaceByContext context) withName displayName

    def flattenSpecials: Map[String, BinaryOperatorTree => Expr] = Map(
      "+" -> (t => {
        if (t.lch == t.rch) BinaryOperatorTree(2, t.lch, _ * _) withName "*"
        else
          t.lch.getValueOption().filter(_ == 0).map(_ => t.rch)
            .orElse(t.rch.getValueOption().filter(_ == 0).map(_ => t.lch))
            .getOrElse(t)
      }),
      "-" -> (t => {
        if (t.lch == t.rch) 0
        else t.lch.getValueOption().filter(_ == 0).map(_ => SingleOperatorTree(t.rch, -_) withName "-")
          .orElse(t.rch.getValueOption().filter(_ == 0).map(_ => t.lch))
          .getOrElse(t)
      }),
      "*" -> (t => {
        t.lch.getValueOption().filter(_ == 0)
          .orElse(t.rch.getValueOption().filter(_ == 0))
          .map(_ => ValueLeaf(0))
          .getOrElse(t)
      })
    )

    override def flatten: Expr = flattenOption getOrElse {
      val result = copy(lch = lch.flatten, rch = rch.flatten) withName displayName
      flattenSpecials.get(displayName).map(_(result)).getOrElse(result)
    }

    override def toString: String = s"$displayName($lch, $rch)"

    override def equals(obj: scala.Any): Boolean = {
      if (super.equals(obj)) true
      else obj match {
        case BinaryOperatorTree(`lch`, `rch`, _) | BinaryOperatorTree(`rch`, `lch`, _) if displayName == "*" || displayName == "+"
          => obj.asInstanceOf[BinaryOperatorTree].operatorName == operatorName
        case _ => false
      }
    }
  }

  case class ValueLeaf(override val value: ValueType) extends InstantExpr {
    override def toString: String = value.toString
  }

  case class SingleOperatorTree(node: Expr, op: ValueType => ValueType) extends InstantExpr with WithOperatorName {
    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      node.getValueOption(context).map(op)

    override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
      node computeExpressionOption context map op

    override def collectFreeVariable: Set[String] = node.collectFreeVariable

    override def replaceByContext(context: Map[String, Expr]): Expr =
      copy(node = node replaceByContext context) withName displayName

    override def flatten: Expr =
      flattenOption getOrElse(copy(node = node.flatten) withName displayName)

    override def toString: String = s"$displayName($node)"

    override def equals(obj: scala.Any): Boolean =
      if (super.equals(obj)) true
      else this match {
        case t@SingleOperatorTree(`node`, _) => t.operatorName == operatorName
        case _ => false
      }
  }

  case class FreeVariableLeaf(name: String) extends FreeExpr {
    override def computeValue(context: Map[String, ValueType]): Option[ValueType] =
      context.get(name)

    override def computeExpression(context: Map[String, Expr]): Option[ValueType] =
      context.get(name) flatMap (_.computeExpressionOption(context))

    override def collectFreeVariable: Set[String] = Set(name)

    override def replaceByContext(context: Map[String, Expr]): Expr =
      if(context contains name) context(name)
      else this

    override def toString: String = name
  }

  implicit def fromValue(value: ValueType): ValueLeaf = ValueLeaf(value)
  implicit def fromInteger(value: Int): ValueLeaf = RationalExpr(value)
  implicit def fromDouble(value: Double): ValueLeaf = DoubleValue(value)
  implicit def toRational(value: Int): Rational = RationalExpr(value)
  implicit def toFreeVariable(variable: String): FreeVariableLeaf = FreeVariableLeaf(variable)
}
