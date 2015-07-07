package intro;

import java.util.Map;

public class Add extends BinOp {
    public Add(Expr expr1, Expr expr2) {
        super("+", expr1, expr2);
    }

    public int getPrecedence() {
        return 1;
    }

    public int eval(Map<String, Integer> env) {
        return expr1.eval(env) + expr2.eval(env);
    }

    public Expr simplify() {
        Expr simpleExpr1 = expr1.simplify();
        Expr simpleExpr2 = expr2.simplify();
        if (simpleExpr1 instanceof CstI) {
            int i = ((CstI)simpleExpr1).getValue();
            // 0 + e = e
            if (i == 0)
                return simpleExpr2;
        } else if (simpleExpr2 instanceof CstI) {
            int i = ((CstI)simpleExpr2).getValue();
            // e + 0 = e
            if (i == 0)
                return simpleExpr1;
        }
        return this;
    }
}
