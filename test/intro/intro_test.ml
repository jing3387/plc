open Core.Std
open OUnit2
open Intro

let test_intro =
  "test_intro" >::: [
    "test_lookup" >:: (
      fun _ ->
        assert_equal ~msg:"test_lookup_empty" None (lookup [] "x");
        assert_equal ~msg:"test_lookup_exists" (Some 1)
          (lookup [("x", 1)] "x");
        assert_equal ~msg:"test_lookup_not_exists" None
          (lookup [("x", 1)] "y")
    );

    "test_eval" >:: (
      fun _ ->
        assert_equal ~msg:"test_eval_int" 1 (eval (CstI 1) []);
        assert_equal ~msg:"test_eval_var" 1 (eval (Var "x") [("x", 1)]);
        assert_equal ~msg:"test_eval_op_int" 2
          (eval (Prim ("+", CstI 1, CstI 1)) []);
        assert_equal ~msg:"test_eval_op_var" 2
          (eval (Prim ("+", CstI 1, Var "x")) [("x", 1)]);
        assert_raises ~msg:"test_eval_op_unk" (Failure "unknown primitive /")
          (fun _ -> eval (Prim ("/", CstI 1, CstI 1)) [])
    );

    "test_1.1.i" >:: (
      fun _ ->
        assert_equal ~msg:"test_eval_eq" 1
          (eval (Prim ("==", CstI 1, CstI 1)) []);
        assert_equal ~msg:"test_eval_neq" 0
          (eval (Prim ("==", CstI 1, CstI 0)) []);
        assert_equal ~msg:"test_eval_maxl" 1
          (eval (Prim ("max", CstI 1, CstI 0)) []);
        assert_equal ~msg:"test_eval_maxr" 1
          (eval (Prim ("max", CstI 0, CstI 1)) []);
        assert_equal ~msg:"test_eval_minl" 0
          (eval (Prim ("min", CstI 0, CstI 1)) []);
        assert_equal ~msg:"test_eval_minr" 0
          (eval (Prim ("min", CstI 1, CstI 0)) [])
    );

    "test_1.1.iv" >:: (
      fun _ ->
        assert_equal ~msg:"test_eval_if_eq" 1
          (eval (If ((Prim ("==", CstI 0, CstI 0)), CstI 1, CstI 2)) []);
        assert_equal ~msg:"test_eval_if_eq" 2
          (eval (If ((Prim ("==", CstI 0, CstI 1)), CstI 1, CstI 2)) []);
        assert_equal ~msg:"test_eval_if_zero" 1
          (eval (If (CstI 5, CstI 1, CstI 2)) []);
        assert_equal ~msg:"test_eval_if_zero" 2
          (eval (If (CstI 0, CstI 1, CstI 2)) [])
    );

    "test_1.2.ii" >:: (
      fun _ ->
        assert_equal ~printer:(Printf.sprintf "%s") ~msg:"test_fmt_e1"
          "v - (w + z)" (fmt e1);
        assert_equal ~printer:(Printf.sprintf "%s") ~msg:"test_fmt_e2"
          "2 * (v - (w + z))" (fmt e2);
        assert_equal ~printer:(Printf.sprintf "%s") ~msg:"test_fmt_e3"
          "x + y + z + w" (fmt e3)
    );

    "test_1.2.iv" >:: (
      fun _ ->
        assert_equal ~msg:"test_simplify_add_zerol" (Var "a")
          (simplify (Add (CstI 0, Var "a")));
        assert_equal ~msg:"test_simplify_add_zeror" (Var "a")
          (simplify (Add (Var "a", CstI 0)));
        assert_equal ~msg:"test_simplify_sub_zero" (Var "a")
          (simplify (Sub (Var "a", CstI 0)));
        assert_equal ~msg:"test_simplify_mul_onel" (Var "a")
          (simplify (Mul (CstI 1, Var "a")));
        assert_equal ~msg:"test_simplify_mul_oner" (Var "a")
          (simplify (Mul (Var "a", CstI 1)));
        assert_equal ~msg:"test_simplify_mul_zerol" (CstI 0)
          (simplify (Mul (CstI 0, Var "a")));
        assert_equal ~msg:"test_simplify_mul_zeror" (CstI 0)
          (simplify (Mul (Var "a", CstI 0)));
        assert_equal ~msg:"test_simplify_sub_eq_int" (CstI 0)
          (simplify (Sub (CstI 1, CstI 1)));
        assert_equal ~msg:"test_simplify_sub_neq_int"
          (Sub (CstI 2, CstI 1)) (simplify (Sub (CstI 2, CstI 1)));
        assert_equal ~msg:"test_simplify_sub_eq_var" (CstI 0)
          (simplify (Sub (Var "a", Var "a")));
        assert_equal ~msg:"test_simplify_sub_neq_var" (Sub (Var "a", Var "b"))
          (simplify (Sub (Var "a", Var "b")));
        assert_equal ~msg:"test_simplify_compound" (Var "x")
          (simplify (Mul ((Add (CstI 0, CstI 1)), (Add (Var "x", CstI 0)))))
    );


    "test_1.3" >:: (
      fun ctxt ->
        assert_equal
          ~printer:(Printf.sprintf "%s")
          ~msg:"test_fmt_mul_sub" "(a - b) * c"
          (fmt (Mul (Sub (Var "a", Var "b"), Var "c")));
        assert_equal
          ~printer:(Printf.sprintf "%s")
          ~msg:"test_fmt_sub_mul" "a * b - c"
          (fmt (Sub (Mul (Var "a", Var "b"), Var "c")));
        assert_equal
          ~printer:(Printf.sprintf "%s")
          ~msg:"test_fmt_sub_subl" "a - b - c"
          (fmt (Sub (Sub (Var "a", Var "b"), Var "c")));
        assert_equal
          ~printer:(Printf.sprintf "%s")
          ~msg:"test_fmt_sub_subr" "a - (b - c)"
          (fmt (Sub (Var "a", Sub(Var "b", Var "c"))))
    )
  ]

let () = run_test_tt_main test_intro
