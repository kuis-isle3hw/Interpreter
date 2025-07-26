{
(* Exercise 3.2以降で段階的に拡張された予約語リスト *)
let reservedWords = [
 (* 基本的な制御構造のキーワード *)
 ("else", Parser.ELSE);   (* if式のelse節 *)
 ("false", Parser.FALSE); (* 論理値false *)
 ("if", Parser.IF);       (* 条件式 *)
 ("then", Parser.THEN);   (* if式のthen節 *)
 ("true", Parser.TRUE);   (* 論理値true *)
 ("in", Parser.IN);       (* Exercise 3.2: let式のin節 *)
 ("let", Parser.LET);     (* Exercise 3.2: let式のキーワード *)
 ("rec", Parser.REC);     (* Exercise 3.5.1: 再帰関数定義のキーワード *)
 ("fun", Parser.FUN);     (* Exercise 3.3: 関数定義のキーワード *)
 ("dfun", Parser.DFUN);   (* Exercise 3.4.5: 動的束縛関数のキーワード *)
 ("print_string", Parser.PRINT_STRING); (* 文字列出力のキーワード *)
 
]
}

rule main = parse
 (* 空白文字、タブ、改行文字を無視 *)
 [' ' '\009' '\012' '\n']+     { main lexbuf }

(* 整数リテラル（負数にも対応） *)
| "-"? ['0'-'9']+
   { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

(* 括弧 *)
| "(" { Parser.LPAREN }  (* 左括弧 *)
| ")" { Parser.RPAREN }  (* 右括弧 *)

(* 文終端記号 *)
| ";;" { Parser.SEMISEMI }  (* 文の終端 *)

(* 算術演算子 *)
| "+" { Parser.PLUS }  (* 加算演算子 *)
| "*" { Parser.MULT }  (* 乗算演算子 *)

(* 比較演算子 *)
| "<" { Parser.LT }    (* 小なり演算子 *)
| "=" { Parser.EQ }    (* Exercise 3.3.1: 等価演算子 *)

(* 関数関連記号 *)
| "->" { Parser.RARROW }  (* Exercise 3.3: 関数の型矢印 *)

(* Exercise 3.2.3: 論理演算子（重要：&&と||は&と|より先に記述） *)
| "&&" { Parser.AND }  (* 論理積演算子 - 短絡評価あり *)
| "||" { Parser.OR }   (* 論理和演算子 - 短絡評価あり *)

(* 識別子（変数名や関数名） *)
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
   { let id = Lexing.lexeme lexbuf in
     try
       (* 予約語チェック：予約語の場合は対応するトークンを返す *)
       List.assoc id reservedWords
     with
     _ -> 
       (* 予約語でない場合は識別子として扱う *)
       Parser.ID id
    }
(*文字列用*)
|"^" { Parser.CONCAT } (* s1^s2 *)
|".["{ Parser.DOT_LBRACKET } (* s[i] *)
|"]" { Parser.RBRACKET } (* ] *)
|'"' [^'"']* '"' 
   { let s = Lexing.lexeme lexbuf in
     let len = String.length s in
     Parser.STRINGV (String.sub s 1 (len - 2)) } 

(* コメント（行コメントとブロックコメント） *)
(* ファイル終端 *)
| eof { exit 0 }