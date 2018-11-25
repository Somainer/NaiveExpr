package macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros
import protocols.ExpressionTree.{Expr => Expression, BinaryOperatorTree}
import protocols.ValueType

object NaiveMarco {
  def constructBinaryTree(a: Expression, b: Expression)(fn: String): BinaryOperatorTree = macro constructBinaryTreeImpl
  def constructBinaryTreeImpl(c: blackbox.Context)(a: c.Expr[Expression], b: c.Expr[Expression])(fn: c.Expr[String]): c.universe.Tree = {
    import c.universe._
//    val func = ValueType
    q"""
       BinaryOperatorTree($a, $b, _ + _) withName $fn
     """
  }
}
