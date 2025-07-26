open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  (* 文字列定数のテスト *)
  { input = "\"hello\";;"; expected = StringV "hello" };
  { input = "\"\";;"; expected = StringV "" };
  { input = "\"123\";;"; expected = StringV "123" };
  { input = "\"world!\";;"; expected = StringV "world!" };
  
  (* 文字列連結のテスト *)
  { input = "\"hello\" ^ \"world\";;"; expected = StringV "helloworld" };
  { input = "\"\" ^ \"test\";;"; expected = StringV "test" };
  { input = "\"test\" ^ \"\";;"; expected = StringV "test" };
  { input = "\"a\" ^ \"b\" ^ \"c\";;"; expected = StringV "abc" };
  { input = "(\"hello\" ^ \" \") ^ \"world\";;"; expected = StringV "hello world" };
  
  (* 文字列インデックスアクセスのテスト *)
  { input = "\"abc\".[0];;"; expected = StringV "a" };
  { input = "\"abc\".[1];;"; expected = StringV "b" };
  { input = "\"abc\".[2];;"; expected = StringV "c" };
  { input = "\"hello\".[4];;"; expected = StringV "o" };
  { input = "\"x\".[0];;"; expected = StringV "x" };
  
  (* 変数を使った文字列操作 *)
  { input = "let s = \"test\" in s;;"; expected = StringV "test" };
  { input = "let s1 = \"hello\" in let s2 = \"world\" in s1 ^ s2;;"; expected = StringV "helloworld" };
  { input = "let s = \"abc\" in s.[1];;"; expected = StringV "b" };
  { input = "let s = \"hello\" ^ \"world\" in s.[5];;"; expected = StringV "w" };
  
  (* 複合的なテスト *)
  { input = "(\"ab\" ^ \"cd\").[2];;"; expected = StringV "c" };
  { input = "let s = \"test\" in (s ^ \"123\").[4];;"; expected = StringV "1" };
  { input = "\"x\" ^ (\"abc\".[1]);;"; expected = StringV "xb" };
  
  (* print_string関数のテスト *)
  { input = "print_string \"hello\";;"; expected = StringV "hello" };
  { input = "print_string \"\";;"; expected = StringV "" };
  { input = "let s = \"world\" in print_string s;;"; expected = StringV "world" };
  { input = "print_string (\"hello\" ^ \"world\");;"; expected = StringV "helloworld" };
  { input = "print_string \"abc\".[1];;"; expected = StringV "b" };
];;

let () = ignore(run_test_tt_main (
    "string_operations" >:::
    gen_eval_tests dataset_for_eval
  ))