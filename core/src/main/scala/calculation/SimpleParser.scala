package calculation

import protocols.ExpressionTree._
import protocols.ValueType

import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object SimpleParser extends ImplicitConversions with RegexParsers {

  def number: Parser[Expr] = lift(
    """[-+]?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ {
      _.toDouble
    })

  def factor: Parser[Expr] = number | "(" ~> expr <~ ")"

  def term = times | div

  def times = binaryOperatorTree(factor)("*")(_ * _)

  def div = binaryOperatorTree(factor)("/")(_ / _)

  def infixOperator[A, B](pa: Parser[A], pb: Parser[A])(sep: Parser[B])(f: (A, A) => A): Parser[A] =
    pa ~ rep(sep ~> pb) ^^ {
      case x ~ xs => (x /: xs) {
        case (a, b) => f(a, b)
      }
    }

  def binaryOperatorTree(expression: Parser[Expr])(ps: Parser[String])(fn: (ValueType, ValueType) => ValueType): Parser[Expr] =
    infixOperator(expression, expression)(ps)(BinaryOperatorTree(_, _, fn))

  def lift(p: Parser[Double]): Parser[ValueLeaf] = p map fromDouble

  def add = binaryOperatorTree(term)("+")(_ + _)

  def minus = binaryOperatorTree(term)("-")(_ - _)

  def expr: Parser[Expr] = add | minus
}
