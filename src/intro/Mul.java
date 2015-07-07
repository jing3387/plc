package intro;

import java.util.Map;

public class Mul extends BinOp {
    public Mul(Expr expr1, Expr expr2) {
        super("*", expr1, expr2);
    }

    public int getPrecedence() {
        return 2;
    }

    public int eval(Map<String, Integer> env) {
        return expr1.eval(env) * expr2.eval(env);
    }

    public Expr simplify() {
        Expr simpleExpr1 = expr1.simplify();
        Expr simpleExpr2 = expr2.simplify();
        if (simpleExpr1 instanceof CstI) {
            int i = ((CstI)simpleExpr1).getValue();
            // 0 * e = 0
            if (i == 0)
                return new CstI(0);
            // 1 * e = e
            else if (i == 1) {
                return simpleExpr2;
            }
        } else if (simpleExpr2 instanceof CstI) {
            int i = ((CstI)simpleExpr2).getValue();
            // e * 0 = 0
            if (i == 0)
                return new CstI(0);
            // e * 1 = e
            else if (i == 1)
                return simpleExpr1;
        }
        return this;
    }
}

