open Eval
open Typing
open Syntax

(*REPLのメインループ*)
let rec read_eval_print env tyenv = (*環境と型環境をREPLに渡し保持*)
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in(*入力文字列を構文木に変換*)
  match decl with(*入力の構文木が*)
  | DeclList decls ->(*ex3.3.2:複数のlet宣言に対応*)
      (*複数宣言の場合:各宣言を逐次処理*)
      let (final_env, final_tyenv) = 
        List.fold_left (fun (curr_env, curr_tyenv) (var_id, e) ->(*declsの左から右に順に処理を行う*)
          let single_decl = Decl (var_id, e) in(*個別の宣言をDeclとして扱う*)
          let (ty, new_tyenv) = ty_decl curr_tyenv single_decl in(*型推論と型環境*)
          let (_, new_env, v) = eval_decl curr_env single_decl in(*現在の環境でlet宣言を評価*)
          
          Printf.printf "val %s : " var_id;
          pp_ty ty;
          print_string " = ";
          pp_val v;
          print_newline();
          
          (new_env, new_tyenv)(*最終的な環境と型環境*)
        ) (env, tyenv) decls(*累積値(curr_env,curr_tyenv)の初期値 処理したい値(var_id,e)のリスト*)
      in
      read_eval_print final_env final_tyenv(*次の入力に備える*)
  | _ ->
      (*単一宣言の場合：従来通り*)
    let (ty, tyenv) = ty_decl tyenv decl in (*let宣言のための型推論*)
    match ty with
      TyUnit ->
        let (id, newenv, v) = eval_decl env decl in(*現在の環境で式を評価*)
        
        Printf.printf "%s : " id;
        pp_ty ty;
        print_string " = ";
        pp_val v;
        print_newline();
        (* 型環境を次のループに渡す．let 宣言はまだないので，tyenv を新しくする必要はない． *)
        read_eval_print newenv tyenv
    | _ ->
      let (id, newenv, v) = eval_decl env decl in(*現在の環境で式を評価*)
        
        Printf.printf "val %s : " id;
        pp_ty ty;
        print_string " = ";
        pp_val v;
        print_newline();
        (* 型環境を次のループに渡す．let 宣言はまだないので，tyenv を新しくする必要はない． *)
        read_eval_print newenv tyenv


(*ex3.2.1大域環境の定義*)
let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
      (Environment.extend "x" (IntV 10) 
          (Environment.extend "ii"  (IntV 2)(*2~4追加*)
              (Environment.extend "iii" (IntV 3)
                  (Environment.extend "iv"  (IntV 4)
                      Environment.empty)))))


(*ex4.2.1:initial_envのための型環境を作る*)
let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
      (Environment.extend "x" TyInt
        (Environment.extend "ii" TyInt
            (Environment.extend "iii" TyInt
                (Environment.extend "iv" TyInt Environment.empty)))))