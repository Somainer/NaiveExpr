package protocols

import protocols.Rational.{Infinity, Rational, RationalExpr}

import scala.annotation.tailrec
import scala.language.implicitConversions

object  ExpressionTree {
  trait Expr {
    def +(that: Expr): BinaryOperatorTree = BinaryOperatorTree(this, that, _ + _) withName "+"
    def -(that: Expr): BinaryOperatorTree = BinaryOperatorTree(this, that, _ - _) withName "-"
    def *(that: Expr): BinaryOperatorTree = BinaryOperatorTree(this, that, _ * _) withName "*"
    def /(that: Expr): BinaryOperatorTree = BinaryOperatorTree(this, that, _ / _) withName "/"

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

    def flattenOption: Option[Expr] = getValueOption() map ValueLeaf

    def derivative(onVariable: String): Expr = {
      if (collectFreeVariable.contains(onVariable)) {
        val self = this
        new AbstractExpr {
          override val innerExpr: Expr = self

          val dx = 0.0000001

          override def collectFreeVariable: Set[String] = self.collectFreeVariable

          override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
            if (context contains onVariable) {
              for {
                fx <- self.computeExpressionOption(context.updated(onVariable, context(onVariable) + dx))
                fxMds <- self.computeExpressionOption(context.updated(onVariable, context(onVariable) - dx))
              } yield (fx - fxMds) / 2 / dx
            } else {
              self.computeExpressionOption(context)
            }

          override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
            computeExpressionOption(context.mapValues(ValueLeaf))

          override def replaceByContext(context: Map[String, Expr]): Expr = {
            val preReplace = self.replaceByContext(context.filterKeys(_ != onVariable)).derivative(onVariable)
            if (context contains onVariable) {
              val x = context(onVariable)
              val fx = self.replaceByContext(context.updated(onVariable, x + dx))
              val fxMds = self.replaceByContext(context.updated(onVariable, x - dx))
              (fx - fxMds) / dx / 2
            } else preReplace
          }


          override def flatten: Expr = this

          override def toString: String = s"(d($self)/d$onVariable)"
        }
      } else 0

    }
  }

  trait InstantExpr extends Expr {
    def value: ValueType = getValueOption(Map.empty).get

    override def collectFreeVariable: Set[String] = Set.empty
  }

  abstract class AbstractExpr extends Expr {
    def innerExpr: Expr
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
    def hasSameNameAs(that: String):Boolean = operatorName.length != 0 && operatorName == that
    def hasSameNameAs(that: WithOperatorName): Boolean = hasSameNameAs(that.operatorName)
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

    override def flatten: Expr = flattenOption getOrElse {
      val result = copy(lch = lch.flatten, rch = rch.flatten) withName displayName
      BinaryOperatorTree.flattenSpecials.get(displayName).map(_(result)).getOrElse(result)
    }

    override def toString: String = s"$displayName($lch, $rch)"

    override def equals(obj: scala.Any): Boolean = {
      if (super.equals(obj)) true
      else obj match {
        case BinaryOperatorTree(`lch`, `rch`, _) | BinaryOperatorTree(`rch`, `lch`, _) if displayName == "*" || displayName == "+"
          => obj.asInstanceOf[BinaryOperatorTree] hasSameNameAs this
        case _ => false
      }
    }
  }

  object BinaryOperatorTree {
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
      }),
      "/" -> (t => {
        t.rch.getValueOption().filter(_ == 1).map(_ => t.lch)
          .getOrElse(t)
      }),
      "==" -> (t => {
        if (t.lch == t.rch) true
        else t
      })
    )
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
      else obj match {
        case t@SingleOperatorTree(`node`, _) => t hasSameNameAs this
        case _ => false
      }

  }

  case class IfConditionTree(predicate: Expr, consequent: Expr, alternative: Expr) extends Expr {
    override def collectFreeVariable: Set[String] =
      predicate.getValueOption().map(x => if(x.toBoolean) consequent.collectFreeVariable else alternative.collectFreeVariable) getOrElse {
        predicate.collectFreeVariable ++ consequent.collectFreeVariable ++ alternative.collectFreeVariable
      }

    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      predicate getValueOption context flatMap {
        x => (if (x.toBoolean) consequent else alternative).getValueOption(context)
      }

    override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
      predicate computeExpressionOption context flatMap {
        x => (if (x.toBoolean) consequent else alternative).computeExpressionOption(context)
      }

    override def replaceByContext(context: Map[String, Expr]): Expr =
      IfConditionTree(predicate.replaceByContext(context), consequent replaceByContext context, alternative replaceByContext context)

    override def flatten: Expr =
      predicate.flatten match {
        case ValueLeaf(e) => if(e.toBoolean) consequent.flatten else alternative.flatten
        case els => IfConditionTree(els, consequent.flatten, alternative.flatten)
      }

    override def toString: String = s"(if ($predicate) $consequent else $alternative)"
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

  def findFixPoint[T](start: T)(f: T => T, checker: (T, T) => Boolean, maxIterTime: Int = 100000): Option[T] = {
    @tailrec
    def iter(oldValue: T, newValue: T, maxIterTime: Int):Option[T] =
      if (checker(oldValue, newValue)) Some(newValue)
      else if (maxIterTime < 0) None
      else iter(newValue, f(newValue), maxIterTime - 1)

    iter(start, f(start), maxIterTime)
  }

  def NewtonIter(fn: Double => Double, dfn: Double => Double, guess: Double): Option[Double] = {
    val tolerance = 0.000001
    findFixPoint(guess)(x => x - fn(x)/dfn(x), (x, y) => Math.abs(x - y) < tolerance)
  }

  implicit def handleType[T, U](t: T)(implicit f: T => U): U = f(t)

  case class EquationSolveTree(lhs: Expr, rhs: Expr, toSolve: String) extends Expr {
    override def collectFreeVariable: Set[String] = lhs.collectFreeVariable ++ rhs.collectFreeVariable

    def solveOption(initialGuess: ValueType): Option[ValueType] = {
      if (collectFreeVariable != Set(toSolve)) None
      else {
        val hs = lhs - rhs
        val fr = collectFreeVariable.head
        val dhs = hs.derivative(fr)
        def unlift(fn: ValueType => ValueType): Double => Double = (x: Double) => fn(DoubleValue.DoubleValue(x)).doubleValue
        def toFn(e: Expr): Double => Double = unlift {
          x => e.getValueOption(Map(fr -> x)).get
        }

        NewtonIter(
          toFn(hs),
          toFn(dhs),
          initialGuess.doubleValue
        ).map(handleType[Double, DoubleValue.DoubleValue])

      }
    }

    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] =
      computeExpressionOption(context mapValues ValueLeaf)

    override def computeExpressionOption(context: Map[String, Expr]): Option[ValueType] =
      replaceByContext(context) match {
        case ValueLeaf(v) => Some(v)
        case _ => None
      }

    override def replaceByContext(context: Map[String, Expr]): Expr = {
      val preReplacement = context filterKeys (_ != toSolve)
      val replaced = EquationSolveTree(
        lhs replaceByContext preReplacement,
        rhs replaceByContext preReplacement,
        toSolve
      )
      context
        .get(toSolve)
        .flatMap(_.computeExpressionOption(preReplacement))
        .flatMap(replaced.solveOption(_).map(ValueLeaf).orElse(Some(ValueLeaf(Infinity))))
        .getOrElse(replaced)
    }

    override def toString: String = s"(find $toSolve where ($lhs = $rhs))"
  }

  implicit def fromValue(value: ValueType): ValueLeaf = ValueLeaf(value)
  implicit def fromInteger(value: Int): ValueLeaf = RationalExpr(value)
  implicit def fromDouble(value: Double): ValueLeaf = DoubleValue(value)
  implicit def fromBoolean(value: Boolean): ValueType = BooleanValue(value)
  implicit def treeFromBoolean(value: Boolean): ValueLeaf = fromBoolean(value)
  implicit def toRational(value: Int): Rational = RationalExpr(value)
  implicit def toDoubleValue(value: Double): ValueType = DoubleValue(value)
  implicit def toFreeVariable(variable: String): FreeVariableLeaf = FreeVariableLeaf(variable)
}
