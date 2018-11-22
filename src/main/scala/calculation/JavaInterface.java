package calculation;

import scala.Option;
import scala.util.parsing.combinator.Parsers;

import java.util.HashMap;
import java.util.Map;

public class JavaInterface {
    public static Parsers.ParseResult<ExpressionTree.Expr> compile(String expressionInput) {
        return ExpressionParser.parseAll(ExpressionParser.expression(), expressionInput);
    }

    public static ExpressionTree.Expr forceCompile(String expressionInput) throws IllegalArgumentException {
        Parsers.ParseResult<ExpressionTree.Expr> result = compile(expressionInput);
        if (result.successful()) return result.get();
        throw new IllegalArgumentException("Illegal expression");
    }

    public static String[] getFreeVariables(ExpressionTree.Expr expression) {
        return Helpers.collectFreeVariables(expression);
    }

    public static String computeExpression(ExpressionTree.Expr expression, Map<String, String> context) {
        Option<String> result = Helpers.computeExpression(expression, context).map(ValueType::toString);
        if (result.isDefined()) return result.get();
        throw new IllegalArgumentException("Invalid expression or context");
    }

    public static String computeExpression(ExpressionTree.Expr expression) {
        return computeExpression(expression, new HashMap<>());
    }
}
