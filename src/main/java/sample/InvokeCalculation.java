package sample;
import calculation.JavaInterface;
import calculation.ExpressionTree.Expr;
import calculation.ValueType;

public class InvokeCalculation {
    public static void main(String[] args) {
        Expr e = JavaInterface.forceCompile("(3 + 1) / |4 - 7| * 5");
        ValueType value = JavaInterface.computeExpressionValue(e);
        System.out.println(value.toString() + "(MathO)");
        System.out.println(value.doubleValue() + "(LineO)");
    }
}
