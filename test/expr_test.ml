open Core.Std
open OUnit2
open Expr

let test_expr =
  "test_expr" >::: 
    ["test_lookup" >::
       (fun _ ->
          assert_equal ~msg:"test_lookup_empty" None (lookup [] "x");
          assert_equal ~msg:"test_lookup_exists" (Some 1) 
            (lookup [("x", 1)] "x");
          assert_equal ~msg:"test_lookup_not_exists" None 
            (lookup [("x", 1)] "y"));
     "test_eval" >::
       (fun _ ->
          assert_equal ~msg:"test_eval_int" 1 (eval (CstI 1) []);
          assert_equal ~msg:"test_eval_var" 1 (eval (Var "x") [("x", 1)]);
          assert_equal ~msg:"test_eval_op_int" 2 
            (eval (Prim ("+", CstI 1, CstI 1)) []);
          assert_equal ~msg:"test_eval_op_var" 2
            (eval (Prim ("+", CstI 1, Var "x")) [("x", 1)]);
          assert_raises ~msg:"test_eval_op_unk" (Failure "unknown primitive /")
            (fun _ -> eval (Prim ("/", CstI 1, CstI 1)) []))]

let () = run_test_tt_main test_expr
