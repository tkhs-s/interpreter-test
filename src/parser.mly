%{
open Syntax

(*複数の引数を持つ関数定義式を右結合の入れ子関数に変換する補助関数*)
let rec make_fun_exp args body =(*args:関数引数のリスト,body:関数の本体式*)
  match args with
  | [] -> body
  | x :: xs -> FunExp (x, make_fun_exp xs body)
  (*make_fun_exp ["x"; "y"] (x+y) は FunExp ("x", FunExp ("y", x+y))*)
  (*x+yがBinOp ("+", Var "x", Var "y")として構文木化される*)

%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR(*ex3.2.3:&&,||に対応するトークンAND,OR*)
%token IF THEN ELSE TRUE FALSE
%token LBRACKET RBRACKET SEMICOLON CONS(*ex3.6.2:リスト表記*)
%token CONCAT DOT_LBRACKET PRINT_STRING(*文字列型の値に対応*)
%token PROJ1 PROJ2 COMMA(*ペア型の値に対応*)

%token LET IN EQ (*MiniML2のlet式*)
%token RARROW FUN (*MiniML3の関数抽象*)
%token REC(*MiniML4の再帰的関数定義*)

%token <int> INTV
%token <Syntax.id> ID
%token <string> STRINGV(* "hogehoge" *)

%start toplevel
%type <Syntax.program> toplevel
(*式に関する非終端記号の型宣言*)
%type <Syntax.exp> AExpr ANDExpr AppExpr CONSExpr Expr FunExpr IfExpr LTExpr LetExpr LetRecExpr MExpr ORExpr PExpr
(*リストに関する非終端記号の型宣言*)
%type <(Syntax.id * Syntax.exp) list> let_sequence
%type <Syntax.exp list> list_elements
%type <Syntax.id list> param_list
%type <(Syntax.id * Syntax.exp)> LET_DEF (*新しい補助非終端記号の型*)
%%

toplevel :
    e=Expr SEMISEMI { Exp e }(*単なる式の評価*)
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) } (*MiniML2のlet式*)
  | LET REC f=ID EQ FUN x=ID RARROW e=Expr SEMISEMI { RecDecl (f, x, e) }(*MiniML4の再帰的関数定義*)
  | decl_list=let_sequence SEMISEMI { DeclList (List.rev decl_list) }(*ex3.3.2:let宣言の列 左から右の順になるようにするための逆順*)
  | LET x=ID params=param_list EQ e=Expr SEMISEMI { Decl(x, make_fun_exp params e) } (*ex3.4.3:複数引数の関数let宣言*)
  (*引数と式をmake_fun_exp関数に渡して、その入れ子関数と関数名をDecl型で表すことでlet宣言として処理している*)


(* SEMISEMI競合解消のため、let_sequenceは2つ以上のLET_DEFの並びのみを許容するように変更 *)
let_sequence :
    d1=LET_DEF d2=LET_DEF { [d2; d1] } (*2つ以上のlet宣言の列のケース*)
  | decl_list=let_sequence d=LET_DEF { d :: decl_list } (*3つ以上のlet宣言の列*)

(*新しい補助非終端記号:単一のlet定義*)
LET_DEF :
    LET x=ID EQ e=Expr { (x, e) }(*単一引数*)
  | LET x=ID params=param_list EQ e=Expr { (x, make_fun_exp params e) }(*複数引数*)


(*ex3.4.3*)
param_list :(*複数引数のリスト,補助関数で用いるargsに対応*)
    x=ID { [x] }
  | x=ID params=param_list { x :: params }(*再帰的にparamsに引数を追加している*)

Expr :
    e=IfExpr { e }
  | e=LetExpr { e } (*MiniML2のlet式*)
  | e=ORExpr { e } (*ex3.2.3:もともとはLTExprだったが、&&,||の追加により結合の強さを考慮してORExprに変更*)
  | e=FunExpr { e }  (*MiniML3の関数定義式*)
  | e=LetRecExpr { e }(*MiniML4の再帰的関数定義*)


FunExpr :(*MiniML3の関数定義式*)
(*RARROW競合解消のため、FUN x=ID RARROW e=Expr のルールを削除し、param_list経由に統一*)
    FUN params=param_list RARROW e=Expr { make_fun_exp params e } (*単一引数も複数引数もこのルールで処理*)


LetRecExpr :(*MiniML4の再帰的関数定義*)
    LET REC f=ID EQ FUN x=ID RARROW e1=Expr IN e2 = Expr { LetRecExp (f, x, e1, e2) }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :(*MiniML2のlet式*)
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
  | LET x=ID params=param_list EQ e1=Expr IN e2=Expr { LetExp (x, make_fun_exp params e1, e2) } (* 複数引数のlet式,ex3.4.3 *)

(*||は&&よりも結合が弱いのでORExprの文法規則の記号名でANDExprを用いる。また、左結合であるのでlにORExprを再帰的に用いている。*)
ORExpr :(*ex3.2.3:or*)
    l=ORExpr OR r=ANDExpr { BinOp (Or, l, r) }
  | e=ANDExpr { e }

(*&&は<よりも結合が弱いのでANDExprの文法規則の記号名でLTExprを用いる。また、左結合であるのでlにANDExprを再帰的に用いている。*)
ANDExpr :(*ex3.2.3:and*)
    l=ANDExpr AND r=LTExpr { BinOp (And, l, r) }
  | e=LTExpr { e }

LTExpr :(*<*)
    l=PExpr LT r=LTExpr { BinOp (Lt, l, r) }
  | e=CONSExpr { e }

(*ex3.6.2:cons演算子::の結合は<より強く+より弱い。結合性は右結合*)
CONSExpr :
    e1=PExpr CONS e2=CONSExpr { ConsExp (e1, e2) }
  | e=PExpr { e }

PExpr :(*+*)
    l=PExpr PLUS r=CONCATExpr { BinOp (Plus, l, r) }
  | e=CONCATExpr { e }

CONCATExpr :(* ^ *)
    l=CONCATExpr CONCAT r=MExpr { StrConcatExp (l, r) }
  | e=MExpr { e }

MExpr :(* * *)
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

(*関数適用式は他の演算子よりも結合が強いので演算子の最後に用いられる。また、左結合であるのでe1にAppExprを再帰的に用いている。*)
AppExpr :(*MiniML3の関数適用式*)
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e1=AExpr DOT_LBRACKET e2=Expr RBRACKET { StrGetExp (e1, e2) }(* s.[n] *)
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }
  | LBRACKET l=list_elements RBRACKET { ListExp l }(*ex3.6.2:リスト*)
  | LBRACKET RBRACKET { ListExp [] }(*ex3.6.2:空リスト*)
  (* 文字列型 *)
  | i=STRINGV { SLit i }(* 文字列リテラル *)
  | PRINT_STRING e=AExpr { PrintStrExp e }(* print_string s *)
  (* ペア型 *)
  | LPAREN e1=Expr COMMA e2=Expr RPAREN { PairExp (e1, e2) }
  | PROJ1 e=AExpr { Proj1Exp e }
  | PROJ2 e=AExpr { Proj2Exp e }

(*ex3.6.2:リストの中身*)
list_elements :
    e=Expr { [e] }
  | e=Expr SEMICOLON l=list_elements { e :: l }(*再帰的な表現*)
