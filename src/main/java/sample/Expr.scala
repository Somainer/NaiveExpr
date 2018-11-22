package sample
import calculation._
import calculation.ExpressionTree._
//import moe.roselia.NaiveJSON.JSONParser._
import calculation.ExpressionParser
object Expr {
  import ExpressionParser._
  def main(args: Array[String]): Unit = {
    println(parseAll(expression, "4 * 2x - |3 - 9|").get)
//    def add: (ValueType, ValueType) => ValueType = _ + _
//    def mul: (ValueType, ValueType) => ValueType = _ * _
//    def div: (ValueType, ValueType) => ValueType = _ / _
//    println(BinaryOperatorTree(BinaryOperatorTree(3, 4, add), BinaryOperatorTree(
//      FreeVariableLeaf("name"), FreeVariableLeaf("x"), mul
//    ), div).collectFreeVariable)
    var idx = 0
    val results = scala.collection.mutable.Map.empty[String, Expr]
    while(true) {
      print("NaiveExpr>")
      val jin = new java.util.Scanner(System.in)
      val expr = jin.nextLine()
      if(expr.startsWith("res")) {
        val e = results.get(expr)
        if(e.isEmpty) {
          println("Err")
        } else {
          e.get.collectFreeVariable match {
            case s if s.isEmpty => println(e.get.getValueOption().get)
            case s =>
              val ms = scala.collection.mutable.Map.empty[String, Expr]
              s.toList.foreach(v => {
                print(s"$v?")
                val db = jin.nextLine()
                ms.put(v, parseAll(expression, db).get)
              })
              println(e.get.computeExpressionOption(ms.toMap).get)
          }
        }
      } else {
        val rawRes = parseAll(expression, expr)
        if (rawRes.successful) {
          val res = rawRes.get
          val vars = res.collectFreeVariable
          print(s"res$idx: ")
          if (vars.isEmpty) {
            val computed = res.getValueOption().get
            print(computed.getClass.getSimpleName + " = ")
            println(res.getValueOption().get)
          }
          else {
            print(s"function/${vars.size} = ")
            println(s"(${vars.mkString(", ")}) => $res")
          }
          results.put(s"res$idx", res)
          idx += 1
        } else {
          println("Compile Error!")
        }
      }

    }
//    println(SimpleParser.parseAll(SimpleParser.expr, "1 + (1 - 1)"))
  }
}
