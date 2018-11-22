package sample
import calculation._
import calculation.ExpressionTree._
//import moe.roselia.NaiveJSON.JSONParser._
import calculation.ExpressionParser
object Expr {
  import ExpressionParser._
  def main(args: Array[String]): Unit = {
    println(BinaryOperatorTree(
      3,
      SingleOperatorTree(
        BinaryOperatorTree(1,
          3,
          _ - _),
        _.abs),
      _ ^ _))
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
            case s => {
              val ms = scala.collection.mutable.Map.empty[String, Expr]
              s.toList.foreach(v => {
                print(s"$v?")
                val db = jin.nextLine()
                ms.put(v, parseAll(expression, db).get)
              })
              println(e.get.computeExpressionOption(ms.toMap).get)
            }
          }
        }
      } else {
        val rawRes = parseAll(expression, expr)
        if (rawRes.successful) {
          val res = rawRes.get
          val vars = res.collectFreeVariable
          println(s"res$idx = ")
          if (vars.isEmpty) println(res.getValueOption().get)
          else println(s"(${vars.mkString(",")}) => $expr")
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
