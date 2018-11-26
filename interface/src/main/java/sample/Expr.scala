package sample

import protocols.ExpressionTree._
import protocols.ValueType
//import moe.roselia.NaiveJSON.JSONParser._
import calculation.ExpressionParser

object Expr {

  import ExpressionParser._

  val results = scala.collection.mutable.Map.empty[String, Expr]
  var idx = 0
  var displayWithInfix = true

  private def format(e: Expr) = if(displayWithInfix) e.toInfixString else e.toString

  case class ExternalContext(name: String, expr: Expr) extends Expr {
    override def collectFreeVariable: Set[String] = Set.empty

    override def replaceByContext(context: Map[String, Expr]): ExternalContext =
      ExternalContext(name, expr.replaceByContext(context))

    override def getValueOption(context: Map[String, ValueType]): Option[ValueType] = None

    override def flatten: ExternalContext = ExternalContext(name, expr.flatten)

    override def toInfixString: String = s"def $name = ${expr.toInfixString}"
  }

  case class DeleteResult(name: String) extends Expr {
    override def collectFreeVariable: Set[String] = Set.empty

    override def toInfixString: String = s"undef $name"
  }

  def repl(): Unit = {
    //    println(parseAll(statement, "(4 * 2x - |3 - 9|)(x=1)").map(_.replaceByContext(Map.empty)))
    //    def add: (ValueType, ValueType) => ValueType = _ + _
    //    def mul: (ValueType, ValueType) => ValueType = _ * _
    //    def div: (ValueType, ValueType) => ValueType = _ / _
    //    println(BinaryOperatorTree(BinaryOperatorTree(3, 4, add), BinaryOperatorTree(
    //      FreeVariableLeaf("name"), FreeVariableLeaf("x"), mul
    //    ), div).collectFreeVariable)
    //    println(FreeVariableLeaf("+") == FreeVariableLeaf("+"))
    println(
      """
        | _______         .__             ___________
        | \      \ _____  |__|__  __ ____ \_   _____/__  ________________
        | /   |   \\__  \ |  \  \/ // __ \ |    __)_\  \/  /\____ \_  __ \
        |/    |    \/ __ \|  |\   /\  ___/ |        \>    < |  |_> >  | \/
        |\____|__  (____  /__| \_/  \___  >_______  /__/\_ \|   __/|__|
        |        \/     \/              \/        \/      \/|__|
        |Welcome to NaiveExpr REPL by Somainer (moe.roselia.NaiveExpr).
        |Type in expressions for evaluation. Or try :help.
      """.stripMargin)
    var shouldQuit = false
    val defineStatement = "def" ~> ((identify <~ "=") ~ controlFlow) ^^ {
      case id ~ ex => ExternalContext(id, ex)
    }
    val undefStatement = "undef" ~> identify ^^ DeleteResult
    val grammar = opt(":" ~> identify) ~ opt(defineStatement | undefStatement | controlFlow)
    while (!shouldQuit) {
      print("NaiveExpr>")
      val jin = new java.util.Scanner(System.in)
      val expr = jin.nextLine()
      //      val command = command
      val (rawRes, parseTime) = calculateRuntime(parseAll(grammar, expr))
      if (rawRes.successful) {
        rawRes.get match {
          case Some(x) ~ Some(m) =>
            x match {
              case "raw" => processExpression(m replaceByContext results.toMap, false)
              case "time" =>
                println(s"Command parsed in $parseTime ms.")
                val (_, runTime) = calculateRuntime(processExpression(m replaceByContext results.toMap, true))
                println(s"Evaluated in $runTime ms.")
                println(s"Total ${parseTime + runTime} ms.")
              case "newenv" => processExpression(m, true)
              case s => println(s"Unsupported command $s")
            }
          case None ~ Some(m) => processExpression(m.replaceByContext(results.toMap), true)
          case Some(c) ~ None => c match {
            case "q" | "quit" =>
              println("GoodBye")
              shouldQuit = true
            case "clear" =>
              println("Cleared.")
              results.clear()
              idx = 0
            case "help" => println(
              """
                |:q | :quit => Quit
                |:raw <statement> => Compile statement without flatten
                |<statement> => Compile statement and flatten
                |:help => This message
                |:clear => Clean up all results
                |:time <statement> => Also log calculation time.
                |:newenv <expression> => Supress all defined variables in <expression>
                |
                |def <identity> = <expression> => Define new function/variable <identity>
                |undef <identity> => Remove defined function/variable
                |
                |if (<predicate>) <consequent> else <alternative>
                |
                |<equation> := <expression> ?<identity>= <expression>
                |To solve an equation, give a initial guess.
                |(3x - 2 ?x= 4) => find x where (-(*(3, x), 2) = 4)
                |(3x - 2 ?x= 4)(x = 1) => Solve this equation with initial guess 1 using newton laws.
                |
                |Examples:
                |x
                |Function/1 = (x) => x
                |x - x
                |Number = 0
                |2x + 3y
                |Function/2 = (x, y) => +(*(2, x), *(3, y))
                |(2x + 3y)(x = 1)
                |Function/1 = (y) => +(2, *(3, y))
                |(2x + 3y)(x = 1, y = 2)
                |Number = 8
              """.stripMargin)
            case "infix" =>
              displayWithInfix = true
              println("Switched to infix")
            case "prefix" =>
              displayWithInfix = false
              println("Switched to prefix")
            case s => println(s"Unknown command: $s")
          }
          case None ~ None =>
        }
      } else {
        println("Fatal: Compile Error")
        println(rawRes)
      }


      //    println(SimpleParser.parseAll(SimpleParser.expr, "1 + (1 - 1)"))
    }
  }

  def processExpression(result: Expr, flatten: Boolean): Unit = {
    result match {
      case x: ExternalContext => processExpressionImpl(x, flatten)
      case x: DeleteResult => processExpressionImpl(x, flatten)
      case _ => processExpressionImpl(result, flatten)
    }
  }

  def processExpressionImpl(result: ExternalContext, flatten: Boolean): Unit = {
    val res = if (flatten) result.flatten else result
    results.put(res.name, PartialAppliedExpression(res.expr, Map(res.name -> res.expr)))
    println(s"Defined ${res.name} = ${format(res)}")

  }

  def processExpressionImpl(result: DeleteResult, flatten: Boolean): Unit = {
    if (results.contains(result.name)) {
      results.remove(result.name)
      println(s"Removed defined value: ${result.name}.")
    } else {
      println(s"${result.name} is not defined.")
    }
  }

  def processExpressionImpl(result: Expr, flatten: Boolean): Unit = {
    val res = if (flatten) result.flatten else result
    val vars = res.collectFreeVariable
    //          println(rawRes.get.replaceByContext(results.toMap))
    //          println(res)
    print(s"res$idx: ")
    if (vars.isEmpty) {
      val computed = res.getValueOption().get
      print(computed.typeName + " = ")
      println(res.getValueOption().get)
    }
    else {
      print(s"Function/${vars.size} = ")
      println(s"(${vars.mkString(", ")}) => ${format(res)}")
    }
    results.put(s"res$idx", res)
    results.put("_", res)
    idx += 1
  }

  def main(args: Array[String]): Unit = {
    //    println(parseAll(derivative, "d(x)/dx"))
    if (true || args.length == 1 && args(0) == "repl")
      repl()
    else Main.main(args)
  }

  def calculateRuntime[T](t: => T): (T, Double) = {
    val start = System.nanoTime
    val res = t
    val end = System.nanoTime
    (res, (end - start) / 1000000.0)
  }
}
