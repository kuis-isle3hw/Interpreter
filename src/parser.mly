%{
open Syntax
%}

(* トークン定義 *)
%token LPAREN RPAREN SEMISEMI
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID
%token LET IN EQ
%token RARROW FUN

(* 文字列型関連のトークン *)
%token <string> STRINGV        (* 文字列リテラル *)
%token CONCAT                  (* 文字列連結演算子 ^ *)
%token DOT_LBRACKET RBRACKET   (* 文字列のインデックス取得用 s.[n] *)
%token PRINT_STRING            (* 文字列出力関数 print_string *)

(* ペア型関連のトークン *)
%token COMMA                   (* ペア区切り文字 , *)
%token PROJ1 PROJ2            (* ペア要素取得関数 proj1, proj2 *)

(* 演算子の優先度設定 *)
%left CONCAT                  (* 文字列連結演算子 *)

%start toplevel
%type <Syntax.program> toplevel
%%

(* トップレベル宣言の文法規則 *)
toplevel :
   e=Expr SEMISEMI { Exp e }                           (* 式の評価 *)
 | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }         (* 変数宣言 *)

(* 式の最上位レベル *)
Expr :
   e=IfExpr { e }     (* 条件式 *)
 | e=LetExpr { e }    (* let式 *)
 | e=FunExpr { e }    (* 関数定義式 *)
 | e=ConcatExpr { e } (* 文字列連結式 *)

(* 関数定義式 *)
FunExpr :
   FUN x=ID RARROW e=Expr { FunExp (x, e) }

(* let式 *)
LetExpr :
   LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

(* 条件式 *)
IfExpr :
   IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

(* 文字列連結式 *)
ConcatExpr :
   l=ConcatExpr CONCAT r=AppExpr { StrConcatExp (l, r) }
 | e=AppExpr { e }

(* 関数適用式 *)
AppExpr :
   e1=AppExpr e2=IndexExpr { AppExp (e1, e2) }
 | e=IndexExpr { e }

(* 文字列インデックス式 *)
IndexExpr :
   e=IndexExpr DOT_LBRACKET i=Expr RBRACKET { StrGetExp (e, i) }
 | e=ProjExpr { e }

(* ペア要素取得式 *)
ProjExpr :
   PROJ1 e=AExpr { PairProj1Exp e }
 | PROJ2 e=AExpr { PairProj2Exp e }
 | PRINT_STRING e=AExpr { PrintStringExp e }
 | e=AExpr { e }

(* 原子式（最も優先度が高い） *)
AExpr :
   i=INTV { ILit i }                           (* 整数リテラル *)
 | TRUE   { BLit true }                        (* 真値リテラル *)
 | FALSE  { BLit false }                       (* 偽値リテラル *)
 | i=ID   { Var i }                            (* 変数 *)
 | s=STRINGV { SLit s }                        (* 文字列リテラル *)
 | LPAREN e=Expr RPAREN { e }                  (* 括弧でグループ化 *)
 | LPAREN e1=Expr COMMA e2=Expr RPAREN { PairExp (e1, e2) }  (* ペア作成 *)

