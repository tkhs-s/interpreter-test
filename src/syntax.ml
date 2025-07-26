open MySet
(*変数名*)
type id = string

(*抽象構文木の演算子の型*)
type binOp = Plus | Mult | Lt | And | Or(*ex3.2.3:演算子&&,||の型を加える*)

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp (*MiniML2のlet in式*)
  | FunExp of id * exp (*MiniML3の関数定義式*)
  | AppExp of exp * exp (*MiniML3の関数適用式*)
  | LetRecExp of id * id * exp * exp (*MiniML4の再帰的関数定義 (関数名,関数の引数,関数の本体式,inの後ろの式)*)
  | ListExp of exp list(*ex3.6.2:リスト型*)
  | ConsExp of exp * exp(*ex3.6.2:cons演算子::*)
  (*interpreter-test1*)
  | SLit of string(* "hogehoge" *)
  | StrConcatExp of exp * exp(* s1^s2 *)
  | StrGetExp of exp * exp(* s.[n] *)
  | PrintStrExp of exp (* print_string s *)

type program =
    Exp of exp
  | Decl of id * exp (*MiniML2のlet式("x", 1)*)
  | RecDecl of id * id * exp (*MiniML4の再帰的関数定義*)
  | DeclList of (id * exp) list (*ex3.3.2:複数のlet宣言をまとめた表現[("x", 1); ("y", x+1)]*)

type tyvar = int

type ty =
    TyInt(*ex4.2.1:T-Int,T-Plus,T-Mult用*)
  | TyBool(*T-bool,T-if用*)
  | TyVar of tyvar(*型変数型を表す*)
  | TyFun of ty * ty(*T-Fun(t1,t2)は関数型t1 -> t2を表す*)
  | TyList of ty
  | TyString(* 文字列型対応 *)


let string_of_tyvar n =(*TyVar用の補助関数*)
  let base = Char.code 'a' in(*文字'a'のASCIIコード*)
  let letter = Char.chr (base + (n mod 26)) in(*nを26で割った余りを足して文字指定*)
  let suffix = if n < 26 then "" else string_of_int (n / 26) in(*nが26以上なら数字を追加*)
  "'" ^ String.make 1 letter ^ suffix(*0→'a,25→'z,26→'a1,52→'a2のような変換に*)

(*ex4.2.1:MiniML2型推論*)
let rec pp_ty ty =
  match ty with
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar n -> print_string (string_of_tyvar n)
  | TyFun (t1, t2) ->(*(t1 -> t2)*)
      print_string "(";
      pp_ty t1;
      print_string " -> ";
      pp_ty t2;
      print_string ")"
  | TyList t ->
      print_string "(";
      pp_ty t;
      print_string " list)"
  | TyString -> print_string "string"(* 文字列型 *)

(*ex4.3.1:string_of_ty*)
let rec string_of_ty = function(*ty -> string型の関数*)
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyVar n -> string_of_tyvar n
  | TyFun (t1, t2) ->
      "(" ^ string_of_ty t1 ^ " -> " ^ string_of_ty t2 ^ ")"
  | TyList t ->
      "(" ^ string_of_ty t ^ " list)"
  | TyString -> "string"(* 文字列型 *)

(*呼び出すたびに他とかぶらない新しいtyvar型の値を返す関数、typing.mlにも同じ関数あり *)
let fresh_tyvar =
  let counter = ref 0 in (*次に返すべきtyvar型の値を参照で持っておいて*)
  let body () =
    let v = !counter in
      counter := v + 1; v (*呼び出されたら参照をインクリメントして古いcounterの参照先の値を返す*)
  in body

(*ex4.3.1:freevar_ty*)
(*tyに現れる自由な型変数の識別子(つまりtyvar型の集合)を返す関数を実装せよ*)
(*型はty -> tyvar MySet.t*)
let rec freevar_ty ty =
  match ty with
  | TyInt -> empty(*基本型で型変数なし*)
  | TyBool -> empty
  | TyVar id -> singleton id(*MySetモジュールの関数、要素idだけを含む集合*)
  | TyFun (t1, t2) -> union (freevar_ty t1) (freevar_ty t2)(*集合s1,s2の和集合*)
  | TyList t -> freevar_ty t
  | TyString -> empty(* 文字列型 *)
