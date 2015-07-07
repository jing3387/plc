package intro;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

public class ExprTest {
    private static Map<String, Integer> emptyEnv;
    private static Map<String, Integer> env;

    @org.junit.BeforeClass
    public static void setUp() throws Exception {
        emptyEnv = new HashMap<String, Integer>();
        env = new HashMap<String, Integer>();
        env.put("a", 3);
        env.put("c", 78);
        env.put("baf", 666);
        env.put("b", 111);
    }

    @org.junit.Test
    public void testEvalInt() throws Exception {
        assertEquals(1, (new CstI(1)).eval(emptyEnv));
    }

    @org.junit.Test
    public void testEvalVar() throws Exception {
        assertEquals(3, (new Var("a").eval(env)));
    }

    @org.junit.Test
    public void testEvalOpInt() throws Exception {
        assertEquals(2, (new Add(new CstI(1), new CstI(1))).eval(emptyEnv));
    }

    @org.junit.Test
    public void testEvalOpVar() throws Exception {
        assertEquals(333, (new Mul(new Var("a"), new Var("b"))).eval(env));
    }

    @org.junit.Test
    public void testToString1() throws Exception {
        assertEquals(
            "v - (w + z)",
            (new Sub(
                new Var("v"),
                new Add(new Var("w"), new Var("z"))
            )).toString()
        );
        assertEquals(
            "2 * (v - (w + z))",
            (new Mul (
                new CstI(2),
                new Sub(
                    new Var("v"),
                    new Add(new Var("w"), new Var("z"))
                )
            )).toString()
        );
        assertEquals(
            "x + y + z + w",
            (new Add(
                new Add(
                    new Add(
                        new Var("x"),
                        new Var("y")
                    ),
                    new Var("z")
                ),
                new Var("w")
            )).toString()
        );
    }

    @org.junit.Test
    public void testSimplifyAdd() throws Exception {
        assertTrue(
            new Var("a").equals(
                (new Add(
                    new CstI(0),
                    new Var("a")
                )).simplify()
            )
        );
        assertTrue(
            new Var("a").equals(
                (new Add(
                    new Var("a"),
                    new CstI(0)
                )).simplify()
            )
        );
    }

    @org.junit.Test
    public void testSimplifySub() throws Exception {
        assertTrue(
            new Var("a").equals(
                (new Sub(
                    new Var("a"),
                    new CstI(0)
                )).simplify()
            )
        );
        assertTrue(
            new CstI(0).equals(
                (new Sub(
                    new Var("a"),
                    new Var("a")
                )).simplify()
            )
        );
    }

    @org.junit.Test
    public void testSimplifyMul() throws Exception {
        assertTrue(
            new CstI(0).equals(
                (new Mul(
                    new CstI(0),
                    new Var("a")
                )).simplify()
            )
        );
        assertTrue(
            new CstI(0).equals(
                (new Mul(
                    new Var("a"),
                    new CstI(0)
                )).simplify()
            )
        );
        assertTrue(
            new Var("a").equals(
                (new Mul(
                    new CstI(1),
                    new Var("a")
                )).simplify()
            )
        );
        assertTrue(
            new Var("a").equals(
                (new Mul(
                    new Var("a"),
                    new CstI(1)
                )).simplify()
            )
        );
    }
}