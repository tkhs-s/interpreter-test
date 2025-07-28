open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  (* 基本的なペアのテスト *)
  { input = "(1, 2);;"; expected = PairV (IntV 1, IntV 2) };
  { input = "(true, false);;"; expected = PairV (BoolV true, BoolV false) };
  { input = "(1, \"abc\");;"; expected = PairV (IntV 1, StringV "abc") };
  
  (* 基本的な射影のテスト *)
  { input = "proj1 (1, 2);;"; expected = IntV 1 };
  { input = "proj2 (true, false);;"; expected = BoolV false };
  { input = "proj1 (\"abc\", true);;"; expected = StringV "abc" };
  
  (* ネストしたペアのテスト *)
  { input = "((1, 2), 3);;"; expected = PairV (PairV (IntV 1, IntV 2), IntV 3) };
  { input = "((1, 2), (3, 4));;"; expected = PairV (PairV (IntV 1, IntV 2), PairV (IntV 3, IntV 4)) };
  
  (* ネストしたペアの射影のテスト *)
  { input = "proj1 ((1, 2), 3);;"; expected = PairV (IntV 1, IntV 2) };
  { input = "proj2 ((1, 2), 3);;"; expected = IntV 3 };
  { input = "proj1 ((1, 2), (3, 4));;"; expected = PairV (IntV 1, IntV 2) };
  { input = "proj2 ((1, 2), (3, 4));;"; expected = PairV (IntV 3, IntV 4) };
  
  (* 連続した射影のテスト *)
  { input = "proj1 (proj1 ((1, 2), 3));;"; expected = IntV 1 };
  { input = "proj2 (proj2 (1, (2, 3)));;"; expected = IntV 3 };
  { input = "proj2 (proj1 ((1, 2), (3, 4)));;"; expected = IntV 2 };
  { input = "proj1 (proj2 ((1, 2), (3, 4)));;"; expected = IntV 3 };
  
  (* 算術式を含むペアのテスト *)
  { input = "(1 + 2, 3 * 4);;"; expected = PairV (IntV 3, IntV 12) };
  { input = "proj1 (1 + 2, 3 * 4);;"; expected = IntV 3 };
  
  (* 比較式を含むペアのテスト *)
  { input = "(1 < 2, 3 < 2);;"; expected = PairV (BoolV true, BoolV false) };
  { input = "proj1 (1 < 2, 3 < 2);;"; expected = BoolV true };
  
  (* 変数を使ったペア操作のテスト *)
  { input = "let x = 1 in (x, x + 1);;"; expected = PairV (IntV 1, IntV 2) };
  { input = "let x = (1, 2) in proj1 x;;"; expected = IntV 1 };
  { input = "let x = (3, 4) in (proj1 x + 1, proj2 x * 2);;"; expected = PairV (IntV 4, IntV 8) };
  
  (* 関数とペアの組み合わせテスト *)
  { input = "let f = fun x -> (x, x * 2) in f 3;;"; expected = PairV (IntV 3, IntV 6) };
  { input = "let first = fun x -> proj1 x in first (10, 20);;"; expected = IntV 10 };
  { input = "let swap = fun x -> (proj2 x, proj1 x) in swap (1, 2);;"; expected = PairV (IntV 2, IntV 1) };
  
  (* if式を含むペアのテスト *)
  { input = "if true then (1, 2) else (3, 4);;"; expected = PairV (IntV 1, IntV 2) };
  { input = "(if true then 1 else 2, if false then 3 else 4);;"; expected = PairV (IntV 1, IntV 4) };
  
  (* リストとペアの組み合わせテスト *)
  { input = "[(1, 2); (3, 4)];;"; expected = ConsV (PairV (IntV 1, IntV 2), ConsV (PairV (IntV 3, IntV 4), NilV)) };
  { input = "(1, 2) :: [];;"; expected = ConsV (PairV (IntV 1, IntV 2), NilV) };
];;

let dataset_for_evalerror = [
  (* 型エラーのテスト *)
  { input = "proj1 1;;" };
  { input = "proj2 true;;" };
  { input = "proj1 [];;" };

  (* 型の不整合のテスト *)
  { input = "(1, 2) + 3;;" };
  { input = "if (1, 2) then 3 else 4;;" };
];;

let () = ignore(run_test_tt_main (
    "ex_pair_test" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))