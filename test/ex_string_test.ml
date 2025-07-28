open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  (* 文字列定数のテスト *)
  { input = "\"hello\";;"; expected = StringV "hello" };
  { input = "\"\";;"; expected = StringV "" };
  { input = "\"123\";;"; expected = StringV "123" };
  
  (* 文字列連結のテスト *)
  { input = "\"hello\" ^ \"world\";;"; expected = StringV "helloworld" };
  { input = "\"\" ^ \"test\";;"; expected = StringV "test" };
  { input = "\"a\" ^ \"b\" ^ \"c\";;"; expected = StringV "abc" };
  
  (* n文字目取得のテスト *)
  { input = "\"abc\".[0];;"; expected = StringV "a" };
  { input = "\"abc\".[2];;"; expected = StringV "c" };
  { input = "\"x\".[0];;"; expected = StringV "x" };
  
  (* 変数を使った文字列操作 *)
  { input = "let s = \"test\" in s;;"; expected = StringV "test" };
  { input = "let s1 = \"hello\" in let s2 = \"world\" in s1 ^ s2;;"; expected = StringV "helloworld" };
  { input = "let s = \"abc\" in s.[1];;"; expected = StringV "b" };
  
  (* print_string関数のテスト - unit型を返す *)
  { input = "print_string \"hello\";;"; expected = UnitV };
  { input = "print_string \"\";;"; expected = UnitV };
  { input = "let s = \"world\" in print_string s;;"; expected = UnitV };
  { input = "print_string (\"hello\" ^ \"world\");;"; expected = UnitV };
  { input = "print_string (\"abc\".[1]);;"; expected = UnitV };
  
  (* unit型を使った式のテスト *)
  { input = "let u = print_string \"test\" in u;;"; expected = UnitV };
  
  (* 複雑な文字列操作 *)
  { input = "let prefix = \"Hello\" in let suffix = \"World\" in (prefix ^ \" \") ^ suffix;;"; expected = StringV "Hello World" };
  { input = "let s = \"abcdef\" in s.[0] ^ s.[1] ^ s.[2];;"; expected = StringV "abc" };
  { input = "let s1 = \"abc\" in let s2 = \"def\" in (s1 ^ s2).[3];;"; expected = StringV "d" };
  
  (* 関数と文字列の組み合わせ *)
  { input = "let f = fun x -> x ^ \"!\" in f \"hello\";;"; expected = StringV "hello!" };
  { input = "let concat3 = fun x -> fun y -> fun z -> x ^ y ^ z in concat3 \"a\" \"b\" \"c\";;"; expected = StringV "abc" };
  
  (* リストと文字列の組み合わせ - ConsV構造を使用 *)
  { input = "[\"hello\"; \"world\"];;"; expected = ConsV (StringV "hello", ConsV (StringV "world", NilV)) };
  { input = "\"hello\" :: [];;"; expected = ConsV (StringV "hello", NilV) };
  { input = "\"a\" :: \"b\" :: [];;"; expected = ConsV (StringV "a", ConsV (StringV "b", NilV)) };
  
  (* Cons演算子のテスト *)
  { input = "let head = \"hello\" in let tail = [\"world\"] in head :: tail;;"; expected = ConsV (StringV "hello", ConsV (StringV "world", NilV)) };
  
];;

let dataset_for_evalerror = [
  (* 型のテスト *)
  { input = "2 ^ \"abc\";;"};

  (* print_stringと文字列連結の優先度テスト *)
  { input = "print_string \"hello\" ^ \"world\";;"};
  { input = "(print_string \"test\") ^ \"suffix\";;"};
];;

let () = ignore(run_test_tt_main (
    "ex_string_test" >:::
    gen_eval_tests dataset_for_eval
    @ gen_evalerror_tests dataset_for_evalerror
  ))