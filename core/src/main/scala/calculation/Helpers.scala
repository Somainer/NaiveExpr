package calculation
import protocols.{ExpressionTree, ValueType}

import scala.collection.JavaConverters._

object Helpers {
  def collectFreeVariables(expr: ExpressionTree.Expr): Array[String] = expr.collectFreeVariable.toArray
  def computeExpression(expr: ExpressionTree.Expr, freeVariables: java.util.Map[String, String]): Option[ValueType] =
    expr.computeExpressionOption(freeVariables.asScala.mapValues(JavaInterface.forceCompile).toMap)
}
