# NaiveExpr

![NaiveExpr](https://github.com/Somainer/NaiveExpr/raw/master/naiveexpr.png)

A Naive Expression Evaluator

Run `sample.Expr.main` to enter REPL

```
NaiveExpr>1
res0: Number = 1
NaiveExpr>x
res1: Function/1 = (x) => x
NaiveExpr>x - x
res2: Number = 0
NaiveExpr>(1 / 2) ^ 10
res3: Rational = 1/1024
NaiveExpr>res1(x=233)
res4: Number = 233
NaiveExpr>x + x - y
res5: Function/2 = (x, y) => -(*(2, x), y)
NaiveExpr>_(y=2x)
res6: Number = 0
NaiveExpr>_
res7: Number = 0
NaiveExpr>sinx
res8: Function/1 = (x) => sin(x)
NaiveExpr>|x|
res9: Function/1 = (x) => abs(x)
NaiveExpr>[x]
res10: Function/1 = (x) => int(x)
NaiveExpr>:q
GoodBye
```

An expression is a Float, Number, Rational, or Function

Number/Rational will be auto converted to Float when needed.

We define an expression with free variables as function, 
and it does not behave like functions in programming languages.

To apply a function, just call 
`<function>(<argument 1>=<expression 1>, ..., <argument n>=<expression n>)`
which will substitute arguments with expressions.

`NaiveExpr` has auto currying, partly applied function will be curried to another function.

`res<N>` refers to Nth result

`_` refers to last result

Or use apis
```Scala
import calculation.ExpressionParser._
val expr = "YOUR EXPRESSION"
parseAll(statement, expr).get // Expr
```

If you are using Java, please use Java Interface 

`core.calculation.JavaInterface`
`public static ExpressionAdapter forceCompileBy(ExpressionType type, String expressionInput)`

where ExpressionType is one of:
* EXPRESSION
* STATEMENT
* CONTROLFLOW

You will get a ExpressionAdapter if it is a valid Expression or it will throw an Exception.

Example:

```Java
ExpressionAdapter e = JavaInterface.forceCompileBy(JavaInterface.ExpressionType.CONTROLFLOW, "x + x - y").flatten();
HashMap<String, ExpressionAdapter> hm = new HashMap<>();
hm.put("y", JavaInterface.forceCompileBy(JavaInterface.ExpressionType.CONTROLFLOW, "x + x"));
System.out.println(e.replaceByContext(hm).flatten()); //Should print 0
```