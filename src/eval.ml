type 'a t = (string * 'a) list

exception Not_bound

let empty = []

let extend x v env = (x, v) :: env

let rec lookup x env =
  match env with
  | [] -> raise Not_bound
  | (y, v) :: rest -> if x = y then v else lookup x rest

let map f env = List.map (fun (x, v) -> (x, f v)) env

(* ===== eval.ml ===== *)
open Syntax

type exval = 
  | IntV of int                      (* 整数値 *)
  | StringV of string                (* 文字列値 *)
  | PairV of exval * exval           (* ペア値 *)

exception Error of string

let rec string_of_exval = function
  | IntV i -> string_of_int i
  | StringV s -> "\"" ^ s ^ "\""
  | PairV (v1, v2) -> "(" ^ string_of_exval v1 ^ ", " ^ string_of_exval v2 ^ ")"

let rec eval_exp env = function
  | Var x -> 
      (try Environment.lookup x env 
       with Environment.Not_bound -> raise (Error ("Unbound variable: " ^ x)))
  | ILit i -> IntV i
  | SLit s -> StringV s
  (* 文字列操作 *)
  | StrConcatExp (e1, e2) ->
      let v1 = eval_exp env e1 in
      let v2 = eval_exp env e2 in
      (match v1, v2 with
      | StringV s1, StringV s2 -> StringV (s1 ^ s2)
      | _ -> raise (Error "String concatenation requires string arguments"))
  | StrGetExp (e1, e2) ->
      let v1 = eval_exp env e1 in
      let v2 = eval_exp env e2 in
      (match v1, v2 with
      | StringV s, IntV i ->
          if i >= 0 && i < String.length s then
            StringV (String.make 1 s.[i])
          else
            raise (Error "String index out of bounds")
      | _ -> raise (Error "String indexing requires string and int"))
  | PrintStringExp e ->
      let v = eval_exp env e in
      (match v with
      | StringV s -> 
          print_string s;
          flush_all ();
          StringV s
      | _ -> raise (Error "print_string requires string argument"))
  (* ペア操作 *)
  | PairExp (e1, e2) ->
      let v1 = eval_exp env e1 in
      let v2 = eval_exp env e2 in
      PairV (v1, v2)
  | PairProj1Exp e ->
      let v = eval_exp env e in
      (match v with
      | PairV (v1, _) -> v1
      | _ -> raise (Error "proj1 requires pair argument"))
  | PairProj2Exp e ->
      let v = eval_exp env e in
      (match v with
      | PairV (_, v2) -> v2
      | _ -> raise (Error "proj2 requires pair argument"))

let eval_decl env = function
  | Exp e -> 
      let v = eval_exp env e in
      (env, v)
  | Decl (x, e) ->
      let v = eval_exp env e in
      (Environment.extend x v env, v)
