package frontend
import com.thoughtworks.binding.{Binding, dom}
import org.scalajs.dom.{Event, document, window}
import Binding.{Constant, Var, Vars}
import org.scalajs.dom.html.{Div, Table, TableRow}
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}
import protocols.ExpressionTree.Expr

import scala.language.implicitConversions
import scalajs.js

object NaiveExprApp{
  implicit def makeIntellijHappy(x: scala.xml.Node): Binding[org.scalajs.dom.raw.Node] = ???
  import calculation.ExpressionParser.{parseAll, controlFlow}
  @js.native
  @js.annotation.JSGlobal("M")
  object M extends js.Object {
    def updateTextFields(): Unit = js.native
    def toast(ob: js.Object): Unit = js.native
    def AutoInit(): Unit = js.native
  }

  object Materialize {
    def toast(text: String, externalClasses: String = ""): Unit = {
      M.toast(new js.Object {
        val html: String = text
        val classes: String = externalClasses
      })
    }
  }
  val useInfixNotation = Var(true)
  val inputExpression = Var("")
  def compiledExpression(expr: String) = parseAll(controlFlow, expr)
  val contextMap: Var[Map[String, Expr]] = Var(Map.empty)
  val resultIndex = Var(0)
  val lastCompiledExpression: Var[Option[Expr]] = Var(None)
  def formatExpr(expr: Expr) = Binding {
    formatExprUnbind(expr, useInfixNotation.bind)
  }
  def formatExprUnbind(expr: Expr, infix: Boolean) = if(infix) expr.toInfixString else expr.toString
  def getExpressionType(expr: Expr) = {
    val freeVariable = expr.collectFreeVariable
    expr.getValueOption().map(_.typeName).getOrElse(s"function(${freeVariable.mkString(", ")})")
  }
  def main(args: Array[String]): Unit = {
    println("""
      | _______         .__             ___________
      | \      \ _____  |__|__  __ ____ \_   _____/__  ________________
      | /   |   \\__  \ |  \  \/ // __ \ |    __)_\  \/  /\____ \_  __ \
      |/    |    \/ __ \|  |\   /\  ___/ |        \>    < |  |_> >  | \/
      |\____|__  (____  /__| \_/  \___  >_______  /__/\_ \|   __/|__|
      |        \/     \/              \/        \/      \/|__|
      |Welcome to NaiveExpr App by Somainer (moe.roselia.NaiveExpr).
      |
    """.stripMargin)
    dom.render(document.getElementById("brave-jewel"), index)
//    Materialize.toast("Loaded!", "red")
    M.AutoInit()
  }

  @dom def index:Binding[Div] =
    <div class="flow-text">
      {navbar.bind}
      <div class="center container">
        <img src="images/naiveexpr.png" class="responsive-img"></img>
        {bodyPart.bind}
      </div>
    </div>


  @dom def navbar = {
    <nav class="cyan navbar navbar-expand-xl navbar-dark bg-dark box-shadow fixed-top">
      <div class="container">
        <a href="./" class="brand-logo blog-title">NaiveExpr</a>
      </div>
    </nav>

  }

  @dom def tableBody(key: String, value: Expr): Binding[TableRow] = {
    <tr>
      <th>{key}</th>
      <th>{getExpressionType(value)}</th>
      <th>{formatExpr(value).bind}</th>
      <th><button class="btn waves-effect pink accent-3" onclick={
      ev: Event => contextMap.value = contextMap.value.filterKeys(_ != key)
      }><i class="material-icons">delete</i></button></th>
    </tr>
  }

  @dom def resultTable: Binding[Table] = {
    val contextBind = Vars(contextMap.bind.toSeq.sortBy(_._1).reverse: _*)
    if (contextBind.value.isEmpty) <table></table>
    else
    <table>
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Value</th>
          <th>Action</th>
        </tr>
      </thead>
      <tbody>
        {for {(k, v) <- contextBind} yield tableBody(k, v).bind}
      </tbody>
    </table>
  }

  @dom def infixPart = {
    <div class="switch">
      <label>
        Prefix
        <input type="checkbox"
               onchange={ev: Event => useInfixNotation.value = ev.target.asInstanceOf[HTMLInputElement].checked}
          checked={useInfixNotation.bind}
        ></input>
          <span class="lever">Use Infix?</span>
        Infix
        </label>
      </div>
  }

  def addLastCompiledExpression = {
    val expr = compiledExpression(inputExpression.value)
    lastCompiledExpression.value = expr.map(Some(_)).getOrElse(None)
    expr.map(e => {
      val computedExpression = e.replaceByContext(contextMap.value).flatten
      contextMap.value = contextMap.value + (s"res${resultIndex.value}" -> computedExpression, "_" -> computedExpression)
      resultIndex.value = resultIndex.value + 1
    })
  }

  @dom def bodyPart: Binding[Div] = {

    <div class="container row">
      <div class="input-field col m12 s12">
        <input
        onchange={ev: Event => inputExpression.value = ev.target.asInstanceOf[HTMLInputElement].value; addLastCompiledExpression}
        id="expression" type="text" class="validate"></input>
        <label for="expression">Expression</label>
      </div>
      <div class="col container">{
        val useInfix = useInfixNotation.bind
        val ce = lastCompiledExpression.bind
        <span class={if(ce.isDefined) "" else "red white-text"}>{
          ce
          .map(e => {
            val computedExpression = e.replaceByContext(contextMap.value).flatten
            val freeVariable = computedExpression.collectFreeVariable
            val expressionType = getExpressionType(computedExpression)
            val currentIndex = resultIndex.value

            s"res$currentIndex: $expressionType = ${
              if(freeVariable.isEmpty) "" else s"(${freeVariable.mkString(",")}) => "
            }${formatExprUnbind(computedExpression, useInfix)}"
            }
          )
          .getOrElse(if(inputExpression.value.isEmpty) "" else s"Invalid input.")}
        </span>
      }</div>
      <div class="container row">
        <div class="col left">{infixPart.bind}</div>
        <div class="col right">
          {if(contextMap.bind.nonEmpty) <button class="red btn waves-effect" onclick={
            ev: Event => {
              contextMap.value = Map.empty
              resultIndex.value = 0
            }
            }>
              <i class="material-icons">delete_forever</i>
            </button>
            else <div></div>}
        </div>
      </div>
      <div class="container">{resultTable.bind}</div>
    </div>
  }

}
