open Eval
open Typing
open Syntax

(* 型の出力関数 *)
let rec pp_ty = function
  | TyInt -> print_string "int"
  | TyString -> print_string "string"
  | TyPair (ty1, ty2) -> 
      print_string "(";
      pp_ty ty1;
      print_string " * ";
      pp_ty ty2;
      print_string ")"
  | TyVar n -> print_string ("'" ^ string_of_int n)

(* 値の出力関数 *)
let rec pp_val = function
  | IntV i -> print_int i
  | StringV s -> print_string ("\"" ^ s ^ "\"")
  | PairV (v1, v2) ->
      print_string "(";
      pp_val v1;
      print_string ", ";
      pp_val v2;
      print_string ")"

let eval_decl_cui env = function
  | Exp e -> 
      let v = eval_exp env e in
      ("it", env, v)  (* 式の場合は "it" という名前にする *)
  | Decl (x, e) ->
      let v = eval_exp env e in
      (x, Environment.extend x v env, v)

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (ty, new_tyenv) = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl_cui env decl in
    Printf.printf "val %s : " id;
    pp_ty ty;
    print_string " = ";
    pp_val v;
    print_newline();
    read_eval_print newenv new_tyenv
  with
  | End_of_file -> print_newline(); exit 0
  | Parsing.Parse_error -> 
      Printf.printf "Parse error\n";
      read_eval_print env tyenv
  | Eval.Error msg ->
      Printf.printf "Error: %s\n" msg;
      read_eval_print env tyenv
  | Typing.Error msg ->
      Printf.printf "Type Error: %s\n" msg;
      read_eval_print env tyenv

(* 文字列・ペア型言語用の初期環境（空でも良い） *)
let initial_env = Environment.empty

let initial_tyenv = Environment.empty