open MySet
(* ML interpreter / type reconstruction *)

(* 識別子は文字列として表現 *)
type id = string

(* Exercise 3.2.3: 二項演算子の型定義 - 論理演算子And, Orを追加 *)
type binOp = Plus | Mult | Lt | And | Or

(* 式の抽象構文木（AST）- 各Exerciseで段階的に拡張 *)
type exp =
   Var of id                                  (* 変数 *)
 | ILit of int                               (* 整数リテラル *) 
 | BLit of bool                              (* 論理値リテラル *)
 | BinOp of binOp * exp * exp                (* 二項演算 *)
 | IfExp of exp * exp * exp                  (* 条件式 *)
 | LetExp of id * exp * exp                  (* Exercise 3.2: let式 *)
 | FunExp of id * exp                        (* Exercise 3.3: 関数定義式（静的束縛） *)
 | DFunExp of id * exp                       (* Exercise 3.4.5: 動的束縛関数 *)
 | AppExp of exp * exp                       (* Exercise 3.3: 関数適用式 *)
 | OpExp of binOp                            (* Exercise 3.4.2: 演算子を第一級オブジェクトとして扱う *)
 | LetRecExp of id * id * exp * exp          (* Exercise 3.5.1: 再帰let式 *)
 (*プログラミング言語処理系レポート*)
 | SLit of string                        (* 文字列リテラル *)
 | StrConcatExp of exp * exp (* 文字列連結式 *)
 | StrGetExp of exp * exp (* 文字列のインデックス取得 *)
 | PrintStrExp of exp (* 文字列の出力式 *)

(* プログラムの型定義 - トップレベルでの宣言形式 *)
type program =
   Exp of exp                                (* 式の評価 *)
 | Decl of id * exp                          (* Exercise 3.2: 変数宣言 *)
 | RecDecl of id * id * exp                  (* Exercise 3.5.1: 再帰関数宣言 *)

(* 型変数は整数で識別 *)
type tyvar = int

(* 型の定義 - 型推論システムで使用 *)
type ty =
   TyInt                                     (* 整数型 *)
 | TyString                                  (* 文字列型 *)
 | TyBool                                    (* 論理値型 *)
 | TyVar of tyvar                            (* 型変数 - 型推論で使用 *)
 | TyFun of ty * ty                          (* 関数型: 引数型 -> 戻り値型 *)
 | TyList of ty                              (* リスト型（将来の拡張用） *)

(* 型変数を文字列に変換する関数 - デバッグ用表示 *)
let string_of_tyvar n =
 let base = Char.code 'a' in
 let letter = Char.chr (base + (n mod 26)) in
 let suffix = if n < 26 then "" else string_of_int (n / 26) in
 "'" ^ String.make 1 letter ^ suffix

(* 型を標準出力に表示する関数 - REPL での型表示用 *)
let rec pp_ty typ =
 match typ with
   TyInt -> print_string "int"
 | TyBool -> print_string "bool"
 | TyVar n -> print_string (string_of_tyvar n)
 | TyFun (t1, t2) ->
     print_string "(";
     pp_ty t1;
     print_string " -> ";
     pp_ty t2;
     print_string ")"
 | TyList t ->
     print_string "(";
     pp_ty t;
     print_string " list)"
 |TyString -> print_string "string" (* 文字列型の表示 *)

(* 型を文字列に変換する関数 - エラーメッセージ等で使用 *)
let rec string_of_ty = function
 | TyInt -> "int"
 | TyBool -> "bool"
 | TyVar n -> string_of_tyvar n
 | TyFun (t1, t2) ->
     "(" ^ string_of_ty t1 ^ " -> " ^ string_of_ty t2 ^ ")"
 | TyList t ->
     "(" ^ string_of_ty t ^ " list)"
  | TyString -> "string" (* 文字列型の文字列表現 *)

(* 新しい型変数を生成する関数 - 型推論で重複を避けるため *)
let fresh_tyvar =
 let counter = ref 0 in (* カウンターで一意性を保証 *)
 let body () =
   let v = !counter in
     counter := v + 1; v (* インクリメントして新しい型変数番号を返す *)
 in body

(* 型推論において型変数の依存関係を解析するために使用 *)
let rec freevar_ty ty =
 match ty with
 | TyInt -> empty                            (* 基本型には型変数なし *)
 | TyBool -> empty                           (* 基本型には型変数なし *)
 | TyVar id -> singleton id                  (* 型変数そのものを集合に追加 *)
 | TyFun (t1, t2) -> union (freevar_ty t1) (freevar_ty t2)  (* 両辺の型変数を結合 *)
 | TyList t -> freevar_ty t                  (* リスト要素型の型変数 *)
 | TyString -> empty                         (* 文字列型には型変数なし *)