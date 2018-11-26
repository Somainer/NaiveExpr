package calculation

import protocols.ExpressionTree._
import protocols.Rational.RationalExpr
import protocols.{BooleanValue, DoubleValue, Rational, ValueType}

import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object ExpressionParser extends RegexParsers with ImplicitConversions {
  def digit: Parser[String] = "(0|[1-9][0-9]*|-[1-9][0-9]*)".r

  def double: Parser[Double] = """([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ (_.toDouble)

  def keyLiterals = Set("def", "undef", "if", "else", "true", "false", "d", "where", "and", "solve", "for")

  def bool = ("true" ^^^ true | "false" ^^^ false) ^^ (BooleanValue(_))

  def boolLeaf = bool ^^ ValueLeaf

  def pi = (caseInsensitive("Pi") | "π") ^^^ Math.PI

  def piValue = pi ^^ (DoubleValue(_))

  def doubleValue: ExpressionParser.Parser[DoubleValue.DoubleValue] = double ^^ (DoubleValue(_))

  def rationalValue: ExpressionParser.Parser[Rational.Rational] = digit map (_.toInt) map (RationalExpr(_))

  def identify = "[A-Za-z_][A-Za-z0-9_]*".r filter (!keyLiterals.contains(_))

  def freeVariable: ExpressionParser.Parser[FreeVariableLeaf] = identify ^^ FreeVariableLeaf

  def leafValue: Parser[Expr] = (doubleValue ||| rationalValue) map ValueLeaf

  def piLeaf = piValue ^^ ValueLeaf

  def eLeaf: ExpressionParser.Parser[ValueLeaf] = caseInsensitive("e") ^^^ Math.E

  def simpleMultiply = (leafValue | bracket) ~ rep1((identify ^^ FreeVariableLeaf) | bracket) ^^ {
    case num ~ expr => (num /: expr) (BinaryOperatorTree(_, _, _ * _) withName "*")
  }

  def constants = piLeaf | eLeaf

  def strictFactor =
    simpleMultiply | leafValue | bracket | calls | boolLeaf | (freeVariable ||| constants) | convertToInt

  def factor = strictFactor // | functionCall

  def bracket = "(" ~> controlFlow <~ ")"

  def convertToInt = surround("[", "]")(expression) ^^ (SingleOperatorTree(_, _.toRationalInt) withName "int")

  def convertToIntByCall = singleCallTree(caseInsensitive("int"))(_.toRationalInt)

  def convertToFloat = singleCallTree(caseInsensitive("float"))(_.toDoubleValue)

  def term: Parser[Expr] = derivative | functionCall | binaryOperatorReducer(factor)(Map(
    "*" -> (_ * _),
    "/" -> (_ / _),
    "^" -> (_ ^ _),
    "&&" -> ((x, y) => if (x.toBoolean) y else x)
  ))

  def negative = "-" ~> term ^^ (SingleOperatorTree(_, -_) withName "-")

  def expression = negative ||| binaryOperatorReducer(term)(Map(
    "+" -> (_ + _),
    "-" -> (_ - _),
    "||" -> ((x, y) => if (x.toBoolean) x else y)
  ))

  def statement: ExpressionParser.Parser[Expr] = compares | expression

  def controlFlow: ExpressionParser.Parser[Expr] = functionCallWhere | equationSolveByFn | equationSolve | ifCondition | statement

  def ifCondition: ExpressionParser.Parser[IfConditionTree] = "if" ~> bracket ~ controlFlow ~ ("else" ~> controlFlow) ^^ {
    case predicate ~ consequent ~ alternative => IfConditionTree(predicate, consequent, alternative)
  }

  def infixOperator[A, B](pa: Parser[A], pb: Parser[A])(sep: Parser[B])(f: (A, A) => A): Parser[A] =
    pa ~ rep(sep ~> pb) ^^ {
      case x ~ xs => (x /: xs) (f)
    }

  def binaryOperatorTree(expression: Parser[Expr])(ps: Parser[String])(fn: (ValueType, ValueType) => ValueType): Parser[Expr] =
    infixOperator(expression, expression)(ps)(BinaryOperatorTree(_, _, fn))

  def strictBinaryOperatorTree[T](name: String, isCaseInsensitive: Boolean = false)(fn: (ValueType, ValueType) => T)
                                 (implicit cnv: T => ValueType): Parser[Expr] = {
    val p: Parser[String] = if (isCaseInsensitive) caseInsensitive(name) else name
    (expression <~ p) ~ expression ^^ {
      case x ~ y => BinaryOperatorTree(x, y, (a, b) => cnv(fn(a, b))) withName name
    }
  }


  def binaryOperatorReducer(expression: Parser[Expr])(fns: Map[String, (ValueType, ValueType) => ValueType]): Parser[Expr] = {
    val sep = fns.keys.map(x => x.named(x)).reduce(_ | _)
    expression ~ rep(sep ~ expression) ^^ {
      case x ~ xs => (x /: xs) {
        case (a, s ~ b) => BinaryOperatorTree(a, b, fns(s)).withName(s)
      }
    }
  }

  def binaryCallTree(name: Parser[String])(fn: (ValueType, ValueType) => ValueType): Parser[BinaryOperatorTree] =
    name ~ ("(" ~> expression <~ ",") ~ (expression <~ ")") ^^ {
      case n ~ a ~ b => BinaryOperatorTree(a, b, fn) withName n.toLowerCase()
    }

  def binaryCallTree[T](name: String, isCaseInsensitive: Boolean = false)(fn: (ValueType, ValueType) => T)
                       (implicit cnv: T => ValueType): Parser[BinaryOperatorTree] = {
    val p: Parser[String] = if (isCaseInsensitive) caseInsensitive(name) else name
    p ~> ("(" ~> expression <~ ",") ~ (expression <~ ")") ^^ {
      case a ~ b => BinaryOperatorTree(a, b, (x, y) => cnv(fn(x, y))) withName name
    }
  }

  def singleCallTree(name: Parser[String])(fn: ValueType => ValueType): Parser[SingleOperatorTree] =
    (((name <~ "(") ~ expression <~ ")") | (name ~ factor)) ^^ {
      case n ~ e => SingleOperatorTree(e, fn) withName n.toLowerCase()
    }

  def caseInsensitive(s: String): Parser[String] = s"(?i)${Regex.quote(s)}".r

  def singleCalls =
    sqrt | abs | sin | cos | tan | ln | convertToIntByCall | convertToFloat

  def binaryCalls = pow | log

  def calls = binaryCalls | singleCalls

  //  def add: ExpressionParser.Parser[Expr] = binaryOperatorTree(term)("+")(_ + _)
  //
  //  def minus: ExpressionParser.Parser[Expr] = binaryOperatorTree(term)("-")(_ - _)

  //  def times: ExpressionParser.Parser[Expr] = binaryOperatorTree(factor)("*")(_ * _)
  //
  //  def div: ExpressionParser.Parser[Expr] = binaryOperatorTree(factor)("/")(_ / _)
  //
  def pow: ExpressionParser.Parser[Expr] = binaryCallTree(caseInsensitive("pow"))(_ ^ _) ^^ (_ withName "^")

  def log: ExpressionParser.Parser[Expr] = binaryCallTree(caseInsensitive("log"))(_ log _)

  def compares = eq | lt | leqt | gt | geqt

  def lt: ExpressionParser.Parser[Expr] = strictBinaryOperatorTree("<")(_ < _)

  def eq: ExpressionParser.Parser[Expr] = strictBinaryOperatorTree("==")(_ == _)

  def leqt = strictBinaryOperatorTree("<=")(_ <= _)

  def gt = strictBinaryOperatorTree(">")(_ > _)

  def geqt = strictBinaryOperatorTree(">=")(_ >= _)

  def sqrt: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("sqrt"))(_.sqrt)

  def abs: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("abs"))(_.abs) | (
    "|" ~> expression <~ "|" ^^ (SingleOperatorTree(_, _.abs) withName "abs")
    )

  def sin: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("sin"))(_.sin)

  def cos: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("cos"))(_.cos)

  def tan: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("tan"))(_.tan)

  def ln: ExpressionParser.Parser[SingleOperatorTree] = singleCallTree(caseInsensitive("ln"))(_.ln)

  def surround[A, B, C](pa: Parser[A], pb: Parser[B])(pc: Parser[C]): Parser[C] = pa ~> pc <~ pb

  def variableAssignment: ExpressionParser.Parser[(String, Expr)] = (identify <~ "=") ~ expression ^^ {
    case ident ~ exp => ident -> exp
  }

  def assignmentCall: ExpressionParser.Parser[Map[String, Expr]] = surround("(", ")") {
    repsep(variableAssignment, ",") ^^ (_.toMap)
  }

  def derivative: ExpressionParser.Parser[Expr] = "d" ~> strictFactor ~ ("/" ~> "d" ~> identify) ^^ {
    case expr ~ x => DerivativeExpr(expr, x)
  }

  def equationSolve: ExpressionParser.Parser[EquationSolveTree] = (expression ~ ("?" ~> identify <~ "=") ~ expression ^^ {
    case lhs ~ variable ~ rhs => EquationSolveTree(lhs, rhs, variable)
  }) | ((expression <~ "?=") ~ expression).filter {
    case l ~ r => (l.collectFreeVariable ++ r.collectFreeVariable).size == 1
  } ^^ {
    case l ~ r => EquationSolveTree(l, r, (l.collectFreeVariable ++ r.collectFreeVariable).head)
  }

  def equationSolveByFn: ExpressionParser.Parser[EquationSolveTree] =
    ("solve" ~> optionalBracket(expression ~ opt("=" ~> expression)).filter {
      case l ~ r => (l.collectFreeVariable ++ r.getOrElse(ValueLeaf(0)).collectFreeVariable).size == 1
    } ^^ {
      case l ~ r => EquationSolveTree(l, r.getOrElse(0), (l.collectFreeVariable ++ r.getOrElse(ValueLeaf(0)).collectFreeVariable).head)
    }) | (
      "solve" ~> optionalBracket(expression ~ opt("=" ~> expression) ~ (("for" | ",") ~> identify)) ^^ {
        case l ~ r ~ x => EquationSolveTree(l, r.getOrElse(0), x)
      }
      )

  def optionalBracket[T](e: Parser[T]): Parser[T] = surround("(", ")")(e) | e

  def functionCall: ExpressionParser.Parser[PartialAppliedExpression] = strictFactor ~ rep1(assignmentCall) ^^ {
    case f ~ as => PartialAppliedExpression(f, as.reduce(_ ++ _))
  }

  def functionCallWhere: ExpressionParser.Parser[PartialAppliedExpression] =
    (equationSolve | equationSolveByFn | expression) ~ "where" ~ optionalBracket(repsep(variableAssignment, "," | "and") ^^ (_.toMap)) ^^ {
      case e ~ _ ~ ass => PartialAppliedExpression(e, ass)
    }

}