package intro;

import java.util.Map;

public abstract class BinOp implements Expr {
    protected Expr expr1, expr2;
    private String op;

    public BinOp(String op, Expr expr1, Expr expr2) {
        this.op = op;
        this.expr1 = expr1;
        this.expr2 = expr2;
    }

    public boolean equals(Expr other) {
        if (other instanceof BinOp) {
            BinOp o = (BinOp) other;
            if (op.equals(o.op) && expr1.equals(o.expr1) && expr2.equals(o.expr2))
                return true;
        }
        return false;
    }

    public abstract int getPrecedence();

    private String maybeWrap(Expr expr, int pre) {
        String s = expr.toString();
        if (this.getPrecedence() < pre)
            s = "(" + s + ")";
        return s;
    }

    public String toString() {
        return maybeWrap(expr1, expr1.getPrecedence())
            + " " + op + " "
            + maybeWrap(expr2, expr2.getPrecedence() + this.getPrecedence());
    }

    public abstract int eval(Map<String, Integer> env);

    public abstract Expr simplify();
}
