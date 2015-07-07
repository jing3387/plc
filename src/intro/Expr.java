package intro;

import java.util.Map;

public interface Expr {
    boolean equals(Expr other);
    int getPrecedence();
    String toString();
    int eval(Map<String, Integer> env) throws RuntimeException;
    Expr simplify();
}
