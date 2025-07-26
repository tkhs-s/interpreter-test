open OUnit
open EvalTestGenerator
open Miniml.Eval


  (* グループ1: 基本的なペア (1-3番目) *)
let group1_tests = [
  { input = "(1, 2);;"; expected = PairV (IntV 1, IntV 2) };
  { input = "(true, false);;"; expected = PairV (BoolV true, BoolV false) };
  { input = "(1, true);;"; expected = PairV (IntV 1, BoolV true) };
];;

(* グループ2: 基本的な射影 (4-12番目) *)
let group2_tests = [
  { input = "proj1 (1, 2);;"; expected = IntV 1 };
  { input = "proj2 (1, 2);;"; expected = IntV 2 };
  { input = "proj1 (true, false);;"; expected = BoolV true };
  { input = "proj2 (true, false);;"; expected = BoolV false };
  { input = "proj1 (1, true);;"; expected = IntV 1 };
  { input = "proj2 (1, true);;"; expected = BoolV true };
];;

(* グループ3: ネストしたペア (13-15番目) *)
let group3_tests = [
  { input = "((1, 2), 3);;"; expected = PairV (PairV (IntV 1, IntV 2), IntV 3) };
  { input = "(1, (2, 3));;"; expected = PairV (IntV 1, PairV (IntV 2, IntV 3)) };
  { input = "((1, 2), (3, 4));;"; expected = PairV (PairV (IntV 1, IntV 2), PairV (IntV 3, IntV 4)) };
];;

(* グループ4: ネストしたペアの射影 (16-21番目) *)
let group4_tests = [
  { input = "proj1 ((1, 2), 3);;"; expected = PairV (IntV 1, IntV 2) };
  { input = "proj2 ((1, 2), 3);;"; expected = IntV 3 };
  { input = "proj1 (1, (2, 3));;"; expected = IntV 1 };
  { input = "proj2 (1, (2, 3));;"; expected = PairV (IntV 2, IntV 3) };
  { input = "proj1 ((1, 2), (3, 4));;"; expected = PairV (IntV 1, IntV 2) };
  { input = "proj2 ((1, 2), (3, 4));;"; expected = PairV (IntV 3, IntV 4) };
];;

(* グループ5: 連続した射影 (22-29番目) *)
let group5_tests = [
  { input = "proj1 (proj1 ((1, 2), 3));;"; expected = IntV 1 };
  { input = "proj2 (proj1 ((1, 2), 3));;"; expected = IntV 2 };
  { input = "proj1 (proj2 (1, (2, 3)));;"; expected = IntV 2 };
  { input = "proj2 (proj2 (1, (2, 3)));;"; expected = IntV 3 };
  { input = "proj1 (proj1 ((1, 2), (3, 4)));;"; expected = IntV 1 };
  { input = "proj2 (proj1 ((1, 2), (3, 4)));;"; expected = IntV 2 };
  { input = "proj1 (proj2 ((1, 2), (3, 4)));;"; expected = IntV 3 };
  { input = "proj2 (proj2 ((1, 2), (3, 4)));;"; expected = IntV 4 };
];;

(* グループ6: 算術式を含むペア (30-33番目) - 27, 31番目あたりでエラーの可能性 *)
let group6_tests = [
  { input = "(1 + 2, 3 * 4);;"; expected = PairV (IntV 3, IntV 12) };
  
  { input = "proj1 (1 + 2, 3 * 4);;"; expected = IntV 3 };
  { input = "proj2 (1 + 2, 3 * 4);;"; expected = IntV 12 };
];;

(* グループ7: 比較式を含むペア (34-37番目) *)
let group7_tests = [
  { input = "(1 < 2, 3 < 2);;"; expected = PairV (BoolV true, BoolV false) };
  { input = "proj1 (1 < 2, 3 < 2);;"; expected = BoolV true };
  { input = "proj2 (1 < 2, 3 < 2);;"; expected = BoolV false };
];;

(* グループ8: let式を含むペア (38-42番目) - 38番目でエラー確実 *)
let group8_tests = [
  { input = "let x = 1 in (x, x + 1);;"; expected = PairV (IntV 1, IntV 2) };  (* 38番目 *)
  { input = "let x = 5 in (x * 2, x + 3);;"; expected = PairV (IntV 10, IntV 8) };
  { input = "let p = (1, 2) in proj1 p;;"; expected = IntV 1 };
  { input = "let p = (1, 2) in proj2 p;;"; expected = IntV 2 };
  { input = "let p = (3, 4) in (proj1 p + 1, proj2 p * 2);;"; expected = PairV (IntV 4, IntV 8) };
];;

(*let () = ignore(run_test_tt_main (
    "ex1_2" >:::
    gen_eval_tests dataset_for_eval
  ))*)

let () = 
  print_endline "Testing Group 1 (basic pairs)...";
  ignore(run_test_tt_main (
    "group1" >:::
    gen_eval_tests group1_tests
  ));
  
  print_endline "Testing Group 2 (basic projections)...";
  ignore(run_test_tt_main (
    "group2" >:::
    gen_eval_tests group2_tests
  ));
  
  print_endline "Testing Group 2 (basic projections)...";
  ignore(run_test_tt_main (
    "group3" >:::
    gen_eval_tests group2_tests
  ));
  
  print_endline "Testing Group 2 (basic projections)...";
  ignore(run_test_tt_main (
    "group4" >:::
    gen_eval_tests group2_tests
  ));
  
  print_endline "Testing Group 2 (basic projections)...";
  ignore(run_test_tt_main (
    "group5" >:::
    gen_eval_tests group2_tests
  ));
  

  

  print_endline "Testing Group 6 (arithmetic)...";
  ignore(run_test_tt_main (
    "group6" >:::
    gen_eval_tests group6_tests
  ));
  
  print_endline "Testing Group 7 (comparisons)...";
  ignore(run_test_tt_main (
    "group7" >:::
    gen_eval_tests group7_tests
  ));
  
  print_endline "Testing Group 8 (let expressions)...";
  ignore(run_test_tt_main (
    "group8" >:::
    gen_eval_tests group8_tests
  ))