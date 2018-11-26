package calculation;

import protocols.ExpressionTree;
import protocols.ValueType;
import scala.Option;
import scala.util.parsing.combinator.Parsers;

import java.util.HashMap;
import java.util.Map;

public class JavaInterface {
    public enum ExpressionType {
        EXPRESSION,
        STATEMENT,
        CONTROLFLOW
    }
    public static Parsers.ParseResult<ExpressionTree.Expr> compileBy(Parsers.Parser<ExpressionTree.Expr> exprParser, String expressionInput) {
        return ExpressionParser.parseAll(exprParser, expressionInput);
    }

    public static Parsers.ParseResult<ExpressionTree.Expr> compileBy(ExpressionType type, String expressionInput) {
        Parsers.Parser<ExpressionTree.Expr> parser = null;
        switch (type) {
            case EXPRESSION:
                parser = ExpressionParser.expression();
                break;
            case STATEMENT:
                parser = ExpressionParser.statement();
                break;
            case CONTROLFLOW:
                parser = ExpressionParser.controlFlow();
                break;
        }
        return compileBy(parser, expressionInput);
    }

    public static ExpressionAdapter forceCompileBy(ExpressionType type, String expressionInput) {
        return compileBy(type, expressionInput).map(ExpressionAdapter::new).get();
    }

    public static ExpressionAdapter forceCompileBy(Parsers.Parser<ExpressionTree.Expr> exprParser, String expressionInput) {
        return compileBy(exprParser, expressionInput).map(ExpressionAdapter::new).get();
    }

    public static Parsers.ParseResult<ExpressionTree.Expr> compile(String expressionInput) {
        return compileBy(ExpressionParser.expression(), expressionInput);
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
        return computeExpressionValue(expression, context).toString();
    }

    public static String computeExpression(ExpressionTree.Expr expression) {
        return computeExpression(expression, new HashMap<>());
    }

    public static ValueType computeExpressionValue(ExpressionTree.Expr expression, Map<String, String> context) {
        Option<ValueType> result = Helpers.computeExpression(expression, context);
        if (result.isDefined()) return result.get();
        throw new IllegalArgumentException("Invalid expression or context");
    }
    public static ValueType computeExpressionValue(ExpressionTree.Expr expression) {
        return computeExpressionValue(expression, new HashMap<>());
    }
}
