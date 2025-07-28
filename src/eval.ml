open Syntax

type exval =
    IntV of int
  | BoolV of bool
  (*MiniML3 クロージャ(パラメータ名・関数本体の式・自由変数の束縛情報の組。関数を表す値である)が作成された時点の環境をデータ構造に含める*)
  | ProcV of id * exp * dnval Environment.t ref(* MiniML4 関数閉包内の環境を参照型で保持するように変更 *)
  | ConsV of exval * exval(*ex3.6.2:リスト*)
  | NilV(*ex3.6.2:空リスト*)
  | StringV of string(* 文字列型 "hogehoge"*)
  | UnitV
  | PairV of exval * exval(* ペア型 *)
and dnval = exval

exception Error of string

let err s = raise (Error s)

(*式を評価したものを文字列に変換し表記*)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (_,_,_) -> "<fun>"
  | ConsV (head, tail) ->(*ex3.6.2:リスト表記,ConsV/NilV構造を文字列に変換して表記*)
      let rec list_to_string acc = function(*acc:今までの文字列の蓄積,第2引数:リスト本体*)
        | NilV -> acc(*[acc]を表記*)
        | ConsV (h, t) -> 
            let h_str = string_of_exval h in(*hを文字列に変換*)
            (match acc with
            | "" -> list_to_string h_str t(*accが空ならそのまま使う*)
            | _ -> list_to_string (acc ^ "; " ^ h_str) t)(*accに何か入っていたら;をつけて連結*)
        | _ -> failwith "Invalid list structure"(*ConsV/NilVでなければエラー*)
      in
      "[" ^ list_to_string "" (ConsV (head, tail)) ^ "]"
  | NilV -> "[]"
  | StringV s -> "\"" ^ s ^ "\""(* 文字列リテラル、sはlexer.mllで""を除いているため新たに""追加 *)
  | UnitV -> "()"(* print_string時の表示 *)
  | PairV (e1, e2) ->
    "(" ^ string_of_exval e1 ^ "," ^ string_of_exval e2 ^ ")"

let pp_val v = print_string (string_of_exval v)

(*二項演算子による演算実行*)
let rec apply_prim op arg1 arg2 =
  match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | _, _, _ -> err ("Unsupported operator in apply_prim")(*ex3.2.3:演算子And,Orの追加によりopのパターンマッチが増えたことによる警告を回避するため追加*)

(*リストをConsV/NilV構造に変換する補助関数*)
let rec list_to_cons = function
  | [] -> NilV
  | h :: t -> ConsV (h, list_to_cons t)
(*list_to_cons [IntV 1; IntV 2; IntV 3]は、ConsV (IntV1, ConsV (IntV2, ConsV (IntV3, NilV)))になる*)

(*構文木が表す式を評価*)
let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  (* 文字列型対応 *)
  | SLit s ->StringV s (* 文字列 *)
  | StrConcatExp (exp1, exp2) ->(* s1^s2 *)
    let s1 = eval_exp env exp1 in
    let s2 = eval_exp env exp2 in
    (match s1, s2 with
      StringV s1, StringV s2 -> StringV (s1 ^ s2)(* どちらもstring型なら連結 *)
    | _ -> err ("Both arguments must be string: ^"))
  | StrGetExp (exp1, exp2) ->(* s.[n] *)
      let s1 = eval_exp env exp1 in
      let s2 = eval_exp env exp2 in
      (match s1, s2 with
        StringV s, IntV n ->(* 型が一致してかつ *)
          if 0 <= n && n < String.length s then(* indexが正しいなら *)
            StringV (String.make 1 s.[n])(* s.[n]を1文字の文字列としてStringVの引数に *)
          else
            err ("string index out of bounds")
      | _ -> err ("type error in string indexing"))
  | PrintStrExp exp ->  (* print_string *)
      let s = eval_exp env exp in
      (match s with
        StringV s -> 
          print_endline s;(* \nつきのprint\string これで改行して見やすくした *)
          flush_all ();(* 画面出力 *)
          UnitV  (* 何もない型 *)
      | _ -> err ("type error in print_string"))
  (* ペア型 *)
  | PairExp (exp1, exp2) ->(* ペア型 *)
    let e1 = eval_exp env exp1 in
    let e2 = eval_exp env exp2 in
    PairV (e1, e2)
  | Proj1Exp exp ->(* 第一要素取得 proj1 e *)
      (match eval_exp env exp with(* expがPairVなら *)
        PairV (v1, _) -> v1
      | _ -> err ("proj1 applied to non-pair value"))
  
  | Proj2Exp exp ->(* 第二要素取得 proj2 e *)
      (match eval_exp env exp with
        PairV (_, v2) -> v2
      | _ -> err ("proj2 applied to non-pair value"))
  (*ex3.6.2:リスト,exp_list:式のリスト*)
  | ListExp exp_list ->
      let evaluated_elements = List.map (eval_exp env) exp_list in(*List.mapでexp_listの各要素をeval_expで評価*)
      list_to_cons evaluated_elements(*dnval型のリストをConsV/NilV構造に*)
  | ConsExp (exp1, exp2) ->(*Cons構文(e1::e2)を処理*)
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    ConsV (arg1, arg2)
  (*ex3.2.3:Parserモジュールが生成する&&を受け取った時の還元時アクションに対応したdnvalを返す*)
  | BinOp (And, exp1, exp2) ->
    (*&&の左側のexp1を評価(再帰的に評価した結果が出てくる)*)
    (match eval_exp env exp1 with
        BoolV false -> BoolV false(*&&の左側がfalseなら無条件にfalse*)
      | BoolV true ->(*&&の左側がtrueなら*)
        (*&&の右側のexp2を評価*)
        (match eval_exp env exp2 with
        BoolV bool1 -> BoolV bool1(*exp2がbool型ならその値が評価結果dnval*)
        | _ -> err ("Right arguments must be boolean: &&"))(*exp2がbool型でないならエラー*)
      | _ -> err ("Left arguments must be boolean: &&"))(*exp1がbool型でないならエラー*)
  (*ex3.2.3:演算子||*)
  | BinOp (Or, exp1, exp2) ->
    (*||の左側のexp1を評価*)
    (match eval_exp env exp1 with
        BoolV true -> BoolV true(*||の左側がtrueなら無条件にtrue*)
      | BoolV false ->(*||の左側がfalseなら*)
        (*||の右側exp2を評価*)
        (match eval_exp env exp2 with
        BoolV bool1 -> BoolV bool1(*exp2がbool型ならその値が評価結果*)
        | _ -> err ("Right arguments must be boolean: ||"))(*exp2がbool型でないならエラー*)
      | _ -> err ("Left arguments must be boolean: ||"))(*exp1がbool型でないならエラー*)
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
      BoolV true -> eval_exp env exp2
    | BoolV false -> eval_exp env exp3
    | _ -> err ("Test expression must be boolean: if"))
  (*MiniML2のlet in式*)
  | LetExp (id, exp1, exp2) ->
    let value = eval_exp env exp1 in(*現在の環境でexp1を評価*)
    eval_exp (Environment.extend id value env) exp2(*exp1の評価結果をidの値として環境に追加してexp2を評価*)
  (*MiniML3の関数定義式:現在の環境envをクロージャ内に保存*)
  | FunExp (id, exp) -> ProcV (id, exp, ref env)(*クロージャは環境を参照型で保持*)
  (*MiniML3の関数適用式*)
  | AppExp (exp1, exp2) ->
    let funval = eval_exp env exp1 in(*関数exp1を現在の環境で評価*)
    let arg = eval_exp env exp2 in(*実引数exp2を現在の環境で評価*)
    (*関数exp1の評価結果をパターンマッチで取り出す*)
    (match funval with
        ProcV (id, body, env') -> (*評価結果が実際にクロージャであれば*)
            (*クロージャ内の環境を取り出して仮引数に対する束縛で拡張*)
            let newenv = Environment.extend id arg !env' in(*環境は参照型なので!を使う*)
              eval_exp newenv body
      | _ ->
        (*評価結果がクロージャでなければ，実行時型エラー*)
        err ("Non-function value is applied"))
  (*MiniML4の再帰的関数定義*)
  | LetRecExp (id, para, exp1, exp2) ->
    let dummyenv = ref Environment.empty in(*ダミーの環境への参照を作る*)
    (*クロージャ(関数閉包)を作り，id(関数名)をこのクロージャに束縛するように現在の環境envを拡張*)
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in(*para:関数の引数*)
    (*ダミーの環境への参照に，拡張された環境を破壊的代入してバックパッチ*)
        dummyenv := newenv;(*参照だから中身を変えれる*)
        eval_exp newenv exp2


(*ex3.3.2:複数のlet宣言を連続して評価する関数 decls = letのリスト*)
and eval_exp_decl_list env decls =
  let rec loop current_env results = function
    | [] -> 
      (*eval_declの返り値に合わせて評価結果の組を返す*)
      (match results with(*左から順に新しい宣言の評価結果が並んでいる*)
      [] -> ("-", current_env, IntV 0)  (*空リストの場合*)
      | (last_id, last_val) :: _ -> (last_id, current_env, last_val))(*let宣言の列の最後の評価の組を返す(環境のみ今後使う)*)
    | (id, e) :: rest ->
      (*declsの前、つまりlet宣言を古い宣言から順番に評価*)
      let v = eval_exp current_env e in(*現在の環境でeを評価してvを得る*)
      let new_env = Environment.extend id v current_env in(*拡張した環境を作り*)
      loop new_env ((id, v) :: results) rest(*let宣言の評価結果をresults(右から順にlet宣言の列ができる)に蓄積しつつ残りのlet宣言をまた評価*)
  in
  loop env [] decls

(*束縛変数名、拡張後の環境、評価結果の組を返す*)
let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)(*式の評価であるから変数名は名前がないので"-"*)
  | Decl (id, e) ->(*MiniML2のlet式*)
      let v = eval_exp env e in (id, Environment.extend id v env, v)(*拡張後の追加した環境*)
  | RecDecl (id, param, e) ->(*MiniML4の再帰的関数定義(let rec宣言)*)
      (*eval_expと同じ流れ*)
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV(param, e, dummyenv)) env in
      dummyenv := newenv;
      (id, newenv, ProcV(param, e, dummyenv))(*ProcVの中の環境は参照型に*)
  | DeclList decls ->(*ex3.3.2:複数のlet宣言*)
      eval_exp_decl_list env decls

