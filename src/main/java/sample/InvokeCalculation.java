package sample;
import calculation.JavaInterface;
import calculation.ExpressionTree.Expr;

public class InvokeCalculation {
    public static void main(String[] args) {
        Expr e = JavaInterface.forceCompile("(3 * 1) / 3");
        System.out.println(JavaInterface.computeExpression(e));
    }
}
