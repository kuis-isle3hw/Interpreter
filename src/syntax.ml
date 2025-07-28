(* syntax.ml *)
type id = string

type exp =
  | Var of id                        (* 変数 *)
  | ILit of int                      (* 整数リテラル *)
  | BLit of bool                     (* 真偽値リテラル *)
  | SLit of string                   (* 文字列リテラル *)
  | IfExp of exp * exp * exp         (* 条件式 *)
  | LetExp of id * exp * exp         (* let式 *)
  | FunExp of id * exp               (* 関数定義 *)
  | AppExp of exp * exp              (* 関数適用 *)
  (* 文字列操作 *)
  | StrConcatExp of exp * exp        (* 文字列連結 *)
  | StrGetExp of exp * exp           (* 文字列のn文字目取得 *)
  | PrintStringExp of exp            (* 文字列出力 *)
  (* ペア操作 *)
  | PairExp of exp * exp             (* ペア作成 *)
  | PairProj1Exp of exp              (* ペアの第一要素取得 *)
  | PairProj2Exp of exp              (* ペアの第二要素取得 *)

type program = 
  | Exp of exp                       (* 式 *)
  | Decl of id * exp                 (* 変数宣言 *)