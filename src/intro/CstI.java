package intro;

import java.util.Map;

public class CstI implements Expr {
    private Integer value;

    public CstI(Integer value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    public boolean equals(Expr other) {
        return other instanceof CstI && value.equals(((CstI) other).value);
    }

    public int getPrecedence() {
        return -1;
    }

    public String toString() {
        return Integer.toString(value);
    }

    public int eval(Map<String, Integer> env) {
        return value;
    }

    public Expr simplify() {
        return this;
    }
}
