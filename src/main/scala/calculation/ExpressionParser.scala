package calculation

import calculation.ExpressionTree._
import calculation.Rational.RationalExpr

import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object ExpressionParser extends RegexParsers with ImplicitConversions {
  def digit: Parser[String] = "(0|[1-9][0-9]*|-[1-9][0-9]*)".r

  def double: Parser[Double] = """([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ (_.toDouble)

  def pi = (caseInsensitive("Pi") | "π") ^^^ Math.PI

  def piValue = pi ^^ (DoubleValue(_))

  def doubleValue: ExpressionParser.Parser[DoubleValue.DoubleValue] = double ^^ (DoubleValue(_))

  def rationalValue: ExpressionParser.Parser[Rational.Rational] = digit map (_.toInt) map (RationalExpr(_))

  def identify = "[A-Za-z_][A-Za-z0-9_]*".r

  def freeVariable: ExpressionParser.Parser[FreeVariableLeaf] = identify ^^ FreeVariableLeaf

  def leafValue: Parser[Expr] = (doubleValue ||| rationalValue) map ValueLeaf

  def piLeaf = piValue ^^ ValueLeaf

  def simpleMultiply = (leafValue ~ identify ^^ {
    case num ~ ident => BinaryOperatorTree(num, FreeVariableLeaf(ident), _ * _) withName "*"
  }) | (leafValue ~ bracket ^^ {
    case num ~ expr => BinaryOperatorTree(num, expr, _ * _).withName("*")
  })

  def factor = simpleMultiply | leafValue | bracket | singleCalls | piLeaf | freeVariable

  def bracket = "(" ~> expression <~ ")"

  def term: Parser[Expr] = binaryOperatorReducer(factor)(Map(
    "*" -> (_ * _),
    "/" -> (_ / _),
    "^" -> (_ ^ _)
  ))

  def negative = "-" ~> term ^^ (SingleOperatorTree(_, -_) withName "-")

  def expression = negative ||| binaryOperatorReducer(term)(Map(
    "+" -> (_ + _),
    "-" -> (_ - _)
  ))

  def infixOperator[A, B](pa: Parser[A], pb: Parser[A])(sep: Parser[B])(f: (A, A) => A): Parser[A] =
    pa ~ rep(sep ~> pb) ^^ {
      case x ~ xs => (x /: xs)(f)
    }

  def binaryOperatorTree(expression: Parser[Expr])(ps: Parser[String])(fn: (ValueType, ValueType) => ValueType): Parser[Expr] =
    infixOperator(expression, expression)(ps)(BinaryOperatorTree(_, _, fn))


  def binaryOperatorReducer(expression: Parser[Expr])(fns: Map[String, (ValueType, ValueType) => ValueType]): Parser[Expr] = {
    val sep = fns.keys.map(x => x.named(x)).reduce(_ | _)
    expression ~ rep(sep ~ expression) ^^ {
      case x ~ xs => (x /: xs) {
        case (a, s ~ b) => BinaryOperatorTree(a, b, fns(s)).withName(s)
      }
    }
  }

  def binaryCallTree(name: Parser[String])(fn: (ValueType, ValueType) => ValueType): Parser[Expr] =
    name ~> ("(" ~> expression <~ ",") ~ (expression <~ ")") ^^ {
      case a ~ b => BinaryOperatorTree(a, b, fn)
    }

  def singleCallTree(name: Parser[String])(fn: ValueType => ValueType): Parser[SingleOperatorTree] =
    ((name ~> "(" ~> expression <~ ")") | (name ~> factor)) ^^ (SingleOperatorTree(_, fn).withName(name.toString()))

  def caseInsensitive(s: String): Parser[String] = s"(?i)${Regex.quote(s)}".r

  def singleCalls = sqrt | abs | sin | cos | tan | pow

//  def add: ExpressionParser.Parser[Expr] = binaryOperatorTree(term)("+")(_ + _)
//
//  def minus: ExpressionParser.Parser[Expr] = binaryOperatorTree(term)("-")(_ - _)

//  def times: ExpressionParser.Parser[Expr] = binaryOperatorTree(factor)("*")(_ * _)
//
//  def div: ExpressionParser.Parser[Expr] = binaryOperatorTree(factor)("/")(_ / _)
//
  def pow: ExpressionParser.Parser[Expr] = binaryCallTree(caseInsensitive("pow"))(_ ^ _)

  def sqrt: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("sqrt"))(_.sqrt)

  def abs: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("abs"))(_.abs) | (
    "|" ~> expression <~ "|" ^^ (SingleOperatorTree(_, _.abs) withName "abs")
    )

  def sin: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("sin"))(_.sin)

  def cos: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("cos"))(_.cos)

  def tan: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("tan"))(_.tan)

}