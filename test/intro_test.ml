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
    )
  ]

let () = run_test_tt_main test_intro
