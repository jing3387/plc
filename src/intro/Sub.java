package intro;

import java.util.Map;

public class Sub extends BinOp {
    public Sub(Expr expr1, Expr expr2) {
        super("-", expr1, expr2);
    }

    public int getPrecedence() {
        return 1;
    }

    public int eval(Map<String, Integer> env) {
        return expr1.eval(env) - expr2.eval(env);
    }

    private boolean equal(Expr e1, Expr e2) {
        if (e1 instanceof CstI && e2 instanceof CstI)
            return ((CstI) e1).getValue() == ((CstI) e2).getValue();
        else
            return e1 instanceof Var && e2 instanceof Var
                && ((Var) e1).getValue().equals(((Var) e2).getValue());
    }

    public Expr simplify() {
        Expr simpleExpr1 = expr1.simplify();
        Expr simpleExpr2 = expr2.simplify();
        if (simpleExpr2 instanceof CstI) {
            int i = ((CstI)simpleExpr2).getValue();
            // e - 0 = e
            if (i == 0)
                return simpleExpr1;
        } else if (simpleExpr1.equals(simpleExpr2)) {
            // e - e = 0
            return new CstI(0);
        }
        return this;
    }
}

