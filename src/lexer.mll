{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN); (*MiniML2のlet式 in*)
  ("let", Parser.LET);(*MiniML2のlet式 let*)
  ("fun", Parser.FUN);(*MiniML3の関数定義式*)
  ("rec", Parser.REC);(*MiniML4の再帰的関数定義*)
  ("print_string", Parser.PRINT_STRING);(* 文字列出力関数 *)
  ("proj1", PROJ1 );(* ペアの第1要素取得 proj1 e *)
  ("proj2", PROJ2 );(* ペアの第2要素取得 proj2 e *)
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "&&" { Parser.AND }(*ex3.2.3:&&*)
| "||" { Parser.OR }(*ex3.2.3:||*)
| "=" { Parser.EQ }(*MiniML2のlet式*)
| "->" { Parser.RARROW }(*MiniMl3の関数定義式*)
| "::" { Parser.CONS }(*ex3.6.2:cons演算子*)
| "[" { Parser.LBRACKET }(*ex3.6.2:リスト[*)
| ";" { Parser.SEMICOLON }(*ex3.6.2:リスト;*)
| "]" { Parser.RBRACKET }(*ex3.6.2:リスト]*)
(* 文字列型 *)
| "^" { Parser.CONCAT }(* s1^s2 *)
| ".[" { Parser.DOT_LBRACKET }(* s.[n] *)
| '"' [^ '"']* '"' (* "hogehoge" *)
    { let s = Lexing.lexeme lexbuf in(* 正則表現にマッチした文字列を取り出す(この段階では""付き) *)
      let len = String.length s in(* ""付きの文字列の長さ *)
      Parser.STRINGV (String.sub s 1 (len - 2))(* インデックス0とlen-1(最初と最後の"")を除いた文字列そのものをアクションに *)
    }
(* ペア型 *)
| "," { COMMA } (* (e1, e2) *)

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }
| eof { exit 0 }


