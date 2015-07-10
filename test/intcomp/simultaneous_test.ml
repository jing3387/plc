open Core.Std
open OUnit2
open Simultaneous

let test_simultaneous =
  "test_simultaneous" >::: [
    "test_eval" >:: (
      fun _ ->
        assert_equal ~msg:"test_eval_e1" 34 (eval e1 []);
        assert_equal ~msg:"test_eval_e2" 2217 (eval e2 []);
        assert_equal ~msg:"test_eval_e3" 100 (eval e3 []);
        assert_equal ~msg:"test_eval_e4" 69 (eval e4 []);
        assert_equal ~msg:"test_eval_e5" 14 (eval e5 []);
        assert_raises ~msg:"test_eval_e6" (Failure "x1 not found")
          (fun _ -> eval e6 []);
        assert_equal ~printer:string_of_int ~msg:"test_eval_e7" 34 (eval e7 []);
    );
  ]

let () =
  run_test_tt_main test_simultaneous
