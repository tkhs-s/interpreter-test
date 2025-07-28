open Syntax


exception Error of string

let err s = raise (Error s)(*型エラー時、エラーメッセージと例外発生*)


(*型代入を表す値の型、置換リスト*)
type subst = (tyvar * ty) list

(*Exercise4.3.2*)
let subst_type (subst : (tyvar * ty) list) (ty : ty) : ty =(*substは型変数→型の置換リスト、これをtyに用いてtyを返す型代入の関数*)
  let rec apply_subset (var, ty_subset) ty_target =(*substをtyに適用させる補助関数*)
    match ty_target with
    | TyInt | TyBool | TyString | TyUnit -> ty_target(*基本型はそのまま*)
    | TyVar id ->(
        if id = var then ty_subset(*対象の型変数なら置換*)
        else TyVar id)(*違う型変数ならそのまま*)
    | TyFun (ty1, ty2) ->(*関数型は再帰的に処理*)
        TyFun (apply_subset (var, ty_subset) ty1, apply_subset (var, ty_subset) ty2)
    | TyList t ->(*リスト型も再帰的に処理*)
        TyList (apply_subset (var, ty_subset) t)
    | TyPair (ty1, ty2) ->(* ペア型対応 *)
        TyPair (apply_subset (var, ty_subset) ty1, apply_subset (var, ty_subset) ty2)
  in(*この適用補助関数を置換リストsubstの左から順番に適用*)
  List.fold_left (fun acc_ty subst_pair -> apply_subset subst_pair acc_ty) ty subst(*実行する関数、初期値、リストの並び*)
  (*funの中の第1引数:累積値、第2引数:リストの現在の要素*)


(*型代入を型の等式集合に変換。型の等式制約ty1=ty2は(ty1,ty2)というペアで表現し、等式集合はペアのリストで表現:eqs_of_subst : subst -> (ty * ty) list*)
let eqs_of_subst (s: subst) : (ty * ty) list =
    (*補助関数aux:
    current_subst:まだ処理すべき型代入のリスト
    acc:これまでに生成された型等式制約のリスト(逆順に蓄積)*)
  let rec aux current_subst acc =
    match current_subst with
    | [] ->
        (*全ての型代入を処理し終えたら蓄積されたリストを返す。
        必要であればここで List.rev accをすることで元のsubstの順番に合わせた出力にできるが、
        単一化の入力としては順序は問われないためここでは逆順のまま返す。*)
        acc
    | (id, ty_val) :: rest_subst ->
        (*idが最終的にどのような型になるかを、ty_valに後続の型代入rest_substをすべて適用して求めたresolved_ty_val。
        s = [(beta, TyFun(TyVar alpha, TyInt)); (alpha, TyBool)] の場合、
        (beta, TyFun(TyVar alpha, TyInt)) を処理する際に、
        TyFun(TyVar alpha, TyInt) の中の TyVar alpha が (alpha, TyBool) によって置き換えられることを考慮する。*)
        let resolved_ty_val = subst_type rest_subst ty_val in
        (*生成された等式制約を型等式制約リストに追加する。
        型変数のid(tyvar)はTyVarコンストラクタを使ってty型に変換する必要がある。*)
        aux rest_subst ((TyVar id, resolved_ty_val) :: acc)
  in(*初期呼び出し:与えられたsubstと空リストで開始する*)
  aux s []


(*Exercise4.3.3*)
(*型の等式集合に型代入を適用する関数 subst_eqs: subst -> (ty * ty) list -> (ty * ty) list*)
let subst_eqs (subst: (tyvar * ty) list) (eqs: (ty * ty) list) : (ty * ty) list =
  List.map (fun (t1, t2) -> (subst_type subst t1, subst_type subst t2)) eqs(*与えられたsubstを用いてsubst_type関数をeqsの各要素に適用させたリストを返す*)

(*オカーチェックを行う関数*)
let occur_check alpha ty =
  MySet.member alpha (freevar_ty ty)

(*単一化アルゴリズム*)
let rec unify (eqs: (ty * ty) list) : (tyvar * ty) list =
  match eqs with
  | [] -> [] (*1.空の制約集合:空の型代入を返す*)
  | (t1, t2) :: rest_eqs ->
    match t1, t2 with
    | t1, t2 when t1 = t2 ->(*2.同一の型:int=intやalpha=alphaの場合、残りの制約集合に対して再帰的に単一化を行う*)
      unify rest_eqs
    | TyPair (t1a, t1b), TyPair (t2a, t2b) -> (* ペア型対応 *)
        unify ((t1a, t2a) :: (t1b, t2b) :: rest_eqs)(* 新たに2つの制約(引数同士の等式、返り値同士の等式)を追加 *)
    | TyFun (t1_arg, t1_ret), TyFun (t2_arg, t2_ret) ->(*3.関数型:t1->t2 = t3->t4の場合、新たに2つの制約(引数同士の等式、返り値同士の等式)を追加し、再帰的に単一化*)
        unify ((t1_arg, t2_arg) :: (t1_ret, t2_ret) :: rest_eqs)
    | TyList t1, TyList t2 ->(*4.リスト型:t1 list = t2 listの場合、リストの中身の型に対して制約を追加し、再帰的に単一化*)
        unify ((t1, t2) :: rest_eqs)
    | TyVar alpha, t | t, TyVar alpha ->(*5.型変数とそれ以外の型:alpha=intやalpha = beta->boolの場合。alpha=alphaのケースは2.同一の型で処理される*)
        if occur_check alpha t then(*オカーチェック:型変数alphaがtの中に現れる場合。alpha = alpha->intのような制約は解決できない*)
          err "Unification Error: Occur check failed"
        else
          let s_alpha_tau = [(alpha, t)] in(*新しい型代入 [alpha->t]を生成*)
          let substituted_rest_eqs = subst_eqs s_alpha_tau rest_eqs in(*残りの制約集合rest_eqsに型代入s_alpha_tauを適用*)
          let s_prime = unify substituted_rest_eqs in(*新しい型代入s_alpha_tauを適用した後の残りの制約に対して再帰的に単一化*)
          s_alpha_tau @ s_prime(*結果の型代入を合成:subst_typeはリストの先頭から代入を適用するためs_alpha_tauをs_primeの前に結合する*)
    | _ ->(*6.その他の場合:int=boolやint = alpha->boolなど異なる構造を持つ型の間の等式はエラー*)
        err "Unification Error: Type mismatch"

type tyenv = ty Environment.t(*型環境*)

(*型変数生成機構*)
let tyvar_counter = ref 0

let fresh_tyvar () =(*0から順番に型変数を生成*)
  let n = !tyvar_counter in
  tyvar_counter := n + 1;
  n

(*演算子opが生成すべき制約集合と返り値の型を記述*)
let ty_prim op ty1 ty2 = match op with
    Plus -> ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
  | Mult -> ([ (ty1, TyInt); (ty2, TyInt) ], TyInt)
  | Lt -> ([ (ty1, TyInt); (ty2, TyInt) ], TyBool)
  | And -> ([ (ty1, TyBool); (ty2, TyBool) ], TyBool)
  | Or -> ([ (ty1, TyBool); (ty2, TyBool) ], TyBool)

(*型環境tyenvと式expを受け取って、型代入とexpの型のペアを返す*)
let rec ty_exp tyenv exp =
  match exp with
    Var x ->
      (try ([], Environment.lookup x tyenv) with(*型環境から変数の型を取得*)
          Environment.Not_bound -> err ("variable not bound: " ^ x))(*未束縛ならエラー*)
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  (* 文字列型対応 *)
  | SLit _ -> ([], TyString)
  | StrConcatExp (exp1, exp2) ->(* s1^s2 *)
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    (* 制約: 引数は両方TyStringで結果もTyString *)
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @
              [ (ty1, TyString); (ty2, TyString) ] in
    let s_final = unify eqs in
    (s_final, subst_type s_final ty2)
  | StrGetExp (exp1, exp2) ->(* s.[n]*)
      let (s1, ty1) = ty_exp tyenv exp1 in(* 文字列式の型推論 *)
      let (s2, ty2) = ty_exp tyenv exp2 in(* インデックス式の型推論 *)
      (* 制約:exp1はTyString、exp2はTyIntである必要があり、結果はTyString *)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ 
                [(ty1, TyString); (ty2, TyInt)] in
      let s_final = unify eqs in
      (s_final, TyString)
  | PrintStrExp exp ->(* print_string s *)
      let (s, ty) = ty_exp tyenv exp in(* 引数式の型推論 *)
      (* 制約:引数はstring型である必要があり、結果はunit型 *)
      let eqs = (eqs_of_subst s) @ [(ty, TyString)] in
      let s_final = unify eqs in
      (s_final, TyUnit)
  (* ペア型 *)
  | PairExp (exp1, exp2) ->  (* ペア作成 (e1,e2) *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s_final = unify eqs in
      let ty1_final = subst_type s_final ty1 in
      let ty2_final = subst_type s_final ty2 in
      (s_final, TyPair (ty1_final, ty2_final))
  | Proj1Exp exp ->  (* 第一要素取得 proj1 e *)
      let (s, ty) = ty_exp tyenv exp in
      let ty1 = TyVar (fresh_tyvar ()) in  (* 第一要素の型を表す新しい型変数 *)
      let ty2 = TyVar (fresh_tyvar ()) in  (* 第二要素の型を表す新しい型変数 *)
      (* 制約: expの型はペア型 ty1 * ty2 である必要がある *)
      let eqs = (eqs_of_subst s) @ [(ty, TyPair (ty1, ty2))] in
      let s_final = unify eqs in
      (s_final, subst_type s_final ty1)  (* 第一要素の型を返す *)
  | Proj2Exp exp ->  (* 第二要素取得 proj2 e *)
      let (s, ty) = ty_exp tyenv exp in
      let ty1 = TyVar (fresh_tyvar ()) in  (* 第一要素の型を表す新しい型変数 *)
      let ty2 = TyVar (fresh_tyvar ()) in  (* 第二要素の型を表す新しい型変数 *)
      (* 制約: expの型はペア型 ty1 * ty2 である必要がある *)
      let eqs = (eqs_of_subst s) @ [(ty, TyPair (ty1, ty2))] in
      let s_final = unify eqs in
      (s_final, subst_type s_final ty2)  (* 第二要素の型を返す *)
  | BinOp (op, exp1, exp2) ->(*両方のオペランドの式を再帰的に処理してからty_primで演算を処理*)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in(*s1とs2(型代入subst)を等式制約の集合に変換してeqs3と合わせる*)
      let s3 = unify eqs(*全体の制約をもう一度解く*)
      in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in(*条件式の型推論*)
      let (s2, ty2) = ty_exp tyenv exp2 in(*Then節の型推論*)
      let (s3, ty3) = ty_exp tyenv exp3 in(*Else節の型推論*)
      (* 制約:
      1.条件式(ty1)はTyBoolである必要がある。
      2.Then節(ty2)とElse節(ty3)の型は一致し、If式の結果の型(ty_res)と一致する必要がある。*)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @
                [ (ty1, TyBool);(*条件式はbool型である*)
                  (ty2, ty3)] in(*Then節の型はelse節の型に一致する*)
      let s_final = unify eqs in(*全ての制約を単一化*)
      (s_final, subst_type s_final ty2)(*最終的な型代入と結果の型を返す*)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in(*exp1の型推論*)
      (*idにty1を束縛した新しい型環境を作成し、その下でexp2を型推論する。
      ty1はまだ未解決の型変数を含むかもしれないが、それは後で統合される制約によって解決される。*)
      let new_tyenv = Environment.extend id ty1 tyenv in
      let (s2, ty2) = ty_exp new_tyenv exp2 in(*exp2の型推論*)
      (*s1とs2はそれぞれの部分式で得られた型代入(部分的な解決結果)を表す。
      これらを等式制約に変換し、結合して最終的に単一化する。*)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s_final = unify eqs in(*全ての制約を単一化*)
      (s_final, subst_type s_final ty2)(*最終的な型代入とexp2の型(それがlet式の型となる)を返す*)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in(*idの型を表すfreshな型変数を生成*)
      let s, ranty =(*id:domty でtyenvを拡張しその下でexpを型推論*)
        ty_exp (Environment.extend id domty tyenv) exp in
        (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in(*関数式の型推論*)
      let (s2, ty2) = ty_exp tyenv exp2 in(*引数式の型推論*)
      let ty_res = TyVar (fresh_tyvar ()) in(*関数式の適用結果の型を表す新しい型変数を生成*)
      (*制約:関数式(ty1)の型は、引数式(ty2)の型から結果の型(ty_res)への関数型である必要がある。*)
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyFun(ty2, ty_res))] in
      let s_final = unify eqs in(*全ての制約を単一化*)
      (s_final, subst_type s_final ty_res)(*最終的な型代入と結果の型を返す*)
  | _ -> err "Not Implemented!"
  


let ty_decl tyenv = function
    Exp e ->(*式eの型推論を行い、その結果の型と環境を返す*)
      let (_, ty) = ty_exp tyenv e in (ty, tyenv)
      (*ty_expから返されるtyは、既に型代入sを適用済みの解決された型である。
      ここではty_expが返す最終的な型と、変化しないtyenvを返す。
      sはここではこれ以上使われないためワイルドカード_として捨てる。*)
  | Decl (id, e) ->(*let id = eの形式の宣言の型推論*)
    let (_, ty) = ty_exp tyenv e in
    let new_tyenv = Environment.extend id ty tyenv in(*eの型が解決されたtyでidを環境に拡張する。*)
    (ty, new_tyenv)(*eの型と新しい環境を返す。*)
  | _ -> err "Not Implemented!"
