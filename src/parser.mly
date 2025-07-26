%{
open Syntax
%}

(* トークン定義 *)
%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR (* Exercise 3.2.3: 論理演算子 && と || を追加 *)
%token IF THEN ELSE TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID
%token LET IN EQ
%token REC (* Exercise 3.5.1以降: 再帰関数定義のためのキーワード *)
%token RARROW FUN
%token DFUN  (* Exercise 3.4.5: 動的束縛関数のためのキーワード *)
%token <string> STRINGV (* 文字列リテラル *)
%token CONCAT (* 文字列連結演算子 *)
%token DOT_LBRACKET RBRACKET (* 文字列のインデックス取得用 *)
%token PRINT_STRING (* 文字列出力のキーワード *)

(* 演算子の優先度設定（下に書いたものほど優先度が高い） *)
%left OR        (* Exercise 3.2.3: || が最も優先度が低い（左結合） *)
%left AND       (* Exercise 3.2.3: && が次に優先度が高い（左結合） *)
%left LT EQ     (* 比較演算子 *)
%left PLUS      (* 算術演算子：加算 *)
%left MULT      (* 算術演算子：乗算（最も優先度が高い） *)

%start toplevel
%type <Syntax.program> toplevel
%%

(* トップレベル宣言の文法規則 *)
toplevel :
   e=Expr SEMISEMI { Exp e }  (* 式の評価 *)
 | LET REC f=ID EQ FUN x=ID RARROW e1=Expr SEMISEMI { RecDecl (f, x, e1) }  (* Exercise 3.4: 再帰関数宣言 *)
 | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }  (* 通常の変数宣言 *)
 (* Exercise 3.4.3: 複数引数関数の簡略記法 - トップレベル宣言 *)
 | LET f=ID params=param_list EQ e=Expr SEMISEMI { 
     (* 複数引数をネストした単一引数関数に脱糖（desugaring） *)
     let fun_body = List.fold_right (fun param body -> FunExp (param, body)) params e in
     Decl (f, fun_body) 
   }
 
(* 式の最上位レベル *)
Expr :
 e=LetRecExpr { e }   (* Exercise 3.5.1: let rec式 *)
 | e=IfExpr { e }     (* 条件式 *)
 | e=LetExpr { e }    (* let式 *)
 | e=OrExpr { e }     (* Exercise 3.2.3: 論理和式（最も優先度が低い） *)
 | e=AndExpr { e }    (* Exercise 3.2.3: 論理積式 *)
 | e=FunExpr { e }    (* 関数定義式 *)
 | e=DFunExpr { e }   (* Exercise 3.4.5: 動的束縛関数定義式 *)

(* Exercise 3.4.3: 複数引数のパラメータリスト *)
param_list :
   id=ID { [id] }                           (* 単一引数 *)
 | id=ID rest=param_list { id :: rest }     (* 複数引数：右再帰でリスト構築 *)

(* 関数定義式 *)
FunExpr :
   FUN params=param_list RARROW e=Expr { 
     (* Exercise 3.4.3: 複数引数関数の簡略記法 *)
     (* fun x y z -> e を fun x -> fun y -> fun z -> e に変換 *)
     List.fold_right (fun param body -> FunExp (param, body)) params e 
   }

(* Exercise 3.4.5: 動的束縛関数定義式 *)
DFunExpr :
   DFUN id=ID RARROW e=Expr { DFunExp (id, e) }  (* 環境を保存しない関数 *)
   
(* let式 *)
LetExpr :
   LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }  (* 通常のlet式 *)
 (* Exercise 3.4.3: 複数引数let式の簡略記法 *)
 | LET f=ID params=param_list EQ e1=Expr IN e2=Expr {
     (* let f x y = e1 in e2 を let f = fun x -> fun y -> e1 in e2 に変換 *)
     let fun_body = List.fold_right (fun param body -> FunExp (param, body)) params e1 in
     LetExp (f, fun_body, e2)
   }

(* Exercise 3.5.1: 再帰let式 *)
LetRecExpr :
    LET REC f=ID EQ FUN x=ID RARROW e1=Expr IN e2=Expr { LetRecExp (f, x, e1, e2) }

(* Exercise 3.2.3: 論理和式（|| 演算子） *)
OrExpr :
   l=OrExpr OR r=AndExpr { BinOp (Or, l, r) }  (* 左結合で処理 *)
 | e=AndExpr { e }  (* AndExpr より優先度が低い *)

(* Exercise 3.2.3: 論理積式（&& 演算子） *)
AndExpr :
   l=AndExpr AND r=LTExpr { BinOp (And, l, r) }  (* 左結合で処理 *)
 | e=LTExpr { e }  (* LTExpr より優先度が低い *)

(* 比較演算式 *)
LTExpr :
   l=PExpr LT r=PExpr { BinOp (Lt, l, r) }  (* 小なり比較 *)
 | e=PExpr { e }

(* 加算演算式 *)
PExpr :
   l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }  (* 左結合 *)
 | e=MExpr { e }

CONCATExpr :
   l=CONCATExpr CONCAT r=MExpr { StrConcatExp (l, r) }  (* 文字列連結式 *)
 | e=MExpr { e }

(* 文字列のインデックス取得式 *)

(* 乗算演算式 *)
MExpr :
   l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }  (* 左結合、AppExprより優先度が低い *)
 | e=AppExpr { e }

(* 関数適用式 *)
AppExpr :
   e1=AppExpr e2=AExpr { AppExp (e1, e2) }  (* 左結合で関数適用 *)
 | e=AExpr { e }

(* 原子式（最も優先度が高い） *)
AExpr :
   i=INTV { ILit i }    (* 整数リテラル *)
 | TRUE   { BLit true } (* 真値リテラル *)
 | FALSE  { BLit false }(* 偽値リテラル *)
 | i=ID   { Var i }     (* 変数 *)
 | s=STRINGV { SLit s }  (* 文字列リテラル *)
 | LPAREN e=Expr RPAREN { e }  (* 括弧でグループ化 *)
 (* Exercise 3.4.2: 中置演算子の関数化 - 括弧付き演算子記法 *)
 | LPAREN PLUS RPAREN { OpExp Plus }   (* (+) - 加算演算子を関数として *)
 | LPAREN MULT RPAREN { OpExp Mult }   (* (*) - 乗算演算子を関数として *)*)
 | LPAREN LT RPAREN { OpExp Lt }       (* (<) - 比較演算子を関数として *)
 | LPAREN AND RPAREN { OpExp And }     (* (&&) - 論理積演算子を関数として *)
 | LPAREN OR RPAREN { OpExp Or }       (* (||) - 論理和演算子を関数として *)

(* 条件式 *)
IfExpr :
   IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }  (* if-then-else構文 *)