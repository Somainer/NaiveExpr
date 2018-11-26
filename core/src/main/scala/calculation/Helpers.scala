package calculation
import protocols.{ExpressionTree, ValueType}

import scala.collection.JavaConverters._

object Helpers {
  def collectFreeVariables(expr: ExpressionTree.Expr): Array[String] = expr.collectFreeVariable.toArray
  def computeExpression(expr: ExpressionTree.Expr, freeVariables: java.util.Map[String, String]): Option[ValueType] =
    expr.computeExpressionOption(freeVariables.asScala.mapValues(JavaInterface.forceCompile).toMap)

  def mapToJava[K, V](map: Map[K, V]): java.util.Map[K, V] = map.asJava
  def mapToScala[K, V](map: java.util.Map[K, V]): Map[K, V] = map.asScala.toMap

  def setToJava[V](set: Set[V]): java.util.Set[V] = set.asJava
  def setToScala[V](set: java.util.Set[V]): Set[V] = set.asScala.toSet

}
