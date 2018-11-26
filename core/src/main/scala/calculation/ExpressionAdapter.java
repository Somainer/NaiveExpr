package calculation;
import protocols.ExpressionTree.*;
import protocols.ExpressionTree.Expr;
import protocols.ValueType;

import java.util.Map;
import java.util.Set;

public final class ExpressionAdapter {
    private final Expr expr;

    public ExpressionAdapter(Expr expr) {
        this.expr = expr;
    }

    public Set<String> collectFreeVariable() {
        return Helpers.setToJava(this.expr.collectFreeVariable());
    }

    public ExpressionAdapter replaceByContext(Map<String, ExpressionAdapter> context) {
        return new ExpressionAdapter(this.expr.replaceByContext(Helpers.mapToScala(context).mapValues(v1 -> v1.expr)));
    }

    public ValueType computeExpression(Map<String, String> context) {
        return Helpers.computeExpression(this.expr, context).get();
    }

    public boolean isFunction () {
        return this.collectFreeVariable().size() != 0;
    }

    public ExpressionAdapter flatten() {
        return new ExpressionAdapter(expr.flatten());
    }

    public String getClassName() {
        return expr.getClass().getSimpleName();
    }

    @Override
    public String toString() {
        return expr.toString();
    }
}
