package sample
import calculation._
import calculation.ExpressionTree._
//import moe.roselia.NaiveJSON.JSONParser._
import calculation.ExpressionParser
object Expr {
  import ExpressionParser._
  def main(args: Array[String]): Unit = {
//    println(parseAll(statement, "(4 * 2x - |3 - 9|)(x=1)").map(_.replaceByContext(Map.empty)))
//    def add: (ValueType, ValueType) => ValueType = _ + _
//    def mul: (ValueType, ValueType) => ValueType = _ * _
//    def div: (ValueType, ValueType) => ValueType = _ / _
//    println(BinaryOperatorTree(BinaryOperatorTree(3, 4, add), BinaryOperatorTree(
//      FreeVariableLeaf("name"), FreeVariableLeaf("x"), mul
//    ), div).collectFreeVariable)
    println("NaiveExpr REPL by Somainer :q to quit")
    var idx = 0
    val results = scala.collection.mutable.Map.empty[String, Expr]
    var shouldQuit = false
    while(!shouldQuit) {
      print("NaiveExpr>")
      val jin = new java.util.Scanner(System.in)
      val expr = jin.nextLine()
      if(expr == ":q") {
        println("Goodbye.")
        shouldQuit = true
      } else {
        val rawRes = parseAll(statement, expr)
        if (rawRes.successful) {
          val res = rawRes.get.replaceByContext(results.toMap).flatten
          val vars = res.collectFreeVariable
//          println(res)
          print(s"res$idx: ")
          if (vars.isEmpty) {
            val computed = res.getValueOption().get
            print(computed.getClass.getSimpleName + " = ")
            println(res.getValueOption().get)
          }
          else {
            print(s"Function/${vars.size} = ")
            println(s"(${vars.mkString(", ")}) => $res")
          }
          results.put(s"res$idx", res)
          results.put("_", res)
          idx += 1
        } else {
          println("Compile Error!")
        }
      }

    }
//    println(SimpleParser.parseAll(SimpleParser.expr, "1 + (1 - 1)"))
  }
}
