package sample
import calculation._
import calculation.ExpressionTree._
//import moe.roselia.NaiveJSON.JSONParser._
import calculation.ExpressionParser
object Expr {
  import ExpressionParser._
  val results = scala.collection.mutable.Map.empty[String, Expr]
  var idx = 0
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
        |NaiveExpr REPL by Somainer (moe.roselia.NaiveExpr)
        |Type in expressions for evaluation. Or try :help.
      """.stripMargin)
    var shouldQuit = false
    val grammar = opt(":" ~> identify) ~ opt(statement)
    while(!shouldQuit) {
      print("NaiveExpr>")
      val jin = new java.util.Scanner(System.in)
      val expr = jin.nextLine()
//      val command = command
      val (rawRes, parseTime) = calculateRuntime(parseAll(grammar, expr))
      if (rawRes.successful) {
        rawRes.get match {
          case Some(x) ~ Some(m) =>
            x match {
              case "raw" => processExpression(m replaceByContext results.toMap, true)
              case "time" =>
                println(s"Command parsed in $parseTime ms.")
                val (_, runTime) = calculateRuntime(processExpression(m replaceByContext results.toMap, false))
                println(s"Evaluated in $runTime ms.")
                println(s"Total ${parseTime + runTime} ms.")
              case s => println(s"Unsupported command $s")
            }
          case None ~ Some(m) => processExpression(m.replaceByContext(results.toMap), false)
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
    val res = if(flatten) result else result.flatten
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
      println(s"(${vars.mkString(", ")}) => $res")
    }
    results.put(s"res$idx", res)
    results.put("_", res)
    idx += 1
  }

  def main(args: Array[String]): Unit = {
    repl()
  }

  def calculateRuntime[T](t: => T): (T, Double) = {
    val start = System.nanoTime
    val res = t
    val end = System.nanoTime
    (res, (end - start) / 1000000.0)
  }
}
