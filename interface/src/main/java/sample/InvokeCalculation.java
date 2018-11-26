package sample;

import calculation.ExpressionAdapter;
import calculation.JavaInterface;
import protocols.ValueType;

import java.util.HashMap;

public class InvokeCalculation {
    public static void main(String[] args) {
//        Expr e = JavaInterface.forceCompile("(3 + 1) / |4 - 7| * 5");
        ExpressionAdapter e = JavaInterface.forceCompileBy(JavaInterface.ExpressionType.CONTROLFLOW, "x + x - y").flatten();
        System.out.printf("Compiled Tree(%s): %s\n", e.getClassName(), e.toString());
        if (e.isFunction()) {
            System.out.println(e);
        } else {
            ValueType value = e.computeExpression(new HashMap<>());
            System.out.println(value.toString() + "(MathO)");
            System.out.println(value.doubleValue() + "(LineO)");
        }

    }
}
