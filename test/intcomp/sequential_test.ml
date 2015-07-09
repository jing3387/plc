open Core.Std
open OUnit2
open Sequential

let test_sequential =
  "test_sequential" >::: [
    "test_eval" >:: (
      fun _ ->
        assert_equal ~msg:"test_eval_e1" 34 (eval e1 []);
        assert_equal ~msg:"test_eval_e2" 2217 (eval e2 []);
        assert_equal ~msg:"test_eval_e3" 100 (eval e3 []);
        assert_equal ~msg:"test_eval_e4" 69 (eval e4 []);
        assert_equal ~msg:"test_eval_e5" 14 (eval e5 []);
    );
    "test_eval_seq" >:: (
      fun _ ->
        assert_equal ~msg:"test_eval_e6" 36 (eval e6 [])
    );
    "test_freevars_seq" >:: (
      fun _ ->
        assert_equal ~msg:"test_freevars_seq_e6" [] (freevars e6);
        assert_equal ~msg:"test_freevars_seq_e7" ["x1"] (freevars e7)
    );
    "test_tcomp_seq" >:: (
      fun _ ->
        assert_equal ~msg:"test_tcomp_seq_e1"
          (eval e1 []) (teval (tcomp e1 []) []);
        assert_equal ~msg:"test_tcomp_seq_e2"
          (eval e2 []) (teval (tcomp e2 []) []);
        assert_equal ~msg:"test_tcomp_seq_e3"
          (eval e3 []) (teval (tcomp e3 []) []);
        assert_equal ~msg:"test_tcomp_seq_e4"
          (eval e4 []) (teval (tcomp e4 []) []);
        assert_equal ~msg:"test_tcomp_seq_e5"
          (eval e5 []) (teval (tcomp e5 []) []);
        assert_equal ~msg:"test_tcomp_seq_e6"
          (eval e6 []) (teval (tcomp e6 []) []);
    )
  ]

let () =
  run_test_tt_main test_sequential
