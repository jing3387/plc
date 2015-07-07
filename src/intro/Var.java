package intro;

import java.util.Map;

public class Var implements Expr {
    private String value;

    public Var(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public String toString() {
        return value;
    }

    public boolean equals(Expr other) {
        return other instanceof Var && value.equals(((Var) other).value);
    }

    public int eval(Map<String, Integer> env) {
        Integer i = env.get(value);
        if (i == null)
            throw new RuntimeException(value + " not found");
        return i;
    }

    public int getPrecedence() {
        return -1;
    }

    public Expr simplify() {
        return this;
    }
}
