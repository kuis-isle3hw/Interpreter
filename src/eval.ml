open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | OpV of binOp (* 新規追加：演算子値 *)
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp(* 3.4.5 動的束縛クロージャ - 新規追加 *)
  | StringV of string (* 文字列値  *)
and dnval = exval

exception Error of string
let string_of_binop = function
  | Plus -> "+"
  | Mult -> "*"
  | Lt -> "<"
  | And -> "&&"
  | Or -> "||"

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | OpV op -> "<op:" ^ string_of_binop op ^ ">"    (* 3.4.2新規追加 *)
  | ProcV _ -> "<fun>"  (* 追加 *)
  | DProcV _ -> "<dfun>"    (* 3.4.5 新規追加 *)
  | StringV s -> "\"" ^ s ^ "\"" (* 文字列値の表示 *)

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | SLit s -> StringV s (* 文字列リテラルの評価 *)
  | StrConcatExp (exp1, exp2) ->
    let s1 = eval_exp env exp1 in
    let s2 = eval_exp env exp2 in
    (* 文字列連結の評価 *)
    (match s1, s2 with
     | StringV s1, StringV s2 -> StringV (s1 ^ s2)
     | _ -> err "Both arguments must be strings for concatenation")
  | StrGetExp (exp1, exp2) ->
    let s1 = eval_exp env exp1 in
    let s2 = eval_exp env exp2 in
      (match s1, s2 with
       | StringV s, IntV n ->
           if n >= 0 && n < String.length s then
             StringV (String.make 1 (String.get s n)) (* インデックス取得 *)
           else
             err ("Index out of bounds: " ^ string_of_int n)
       | _ -> err "First argument must be a string and second must be an integer for indexing")
  | PrintStrExp exp ->
    let str_val = eval_exp env exp in
    (match str_val with
      StringV str_val ->
        print_string str_val; (* 文字列を出力 *)
        flush_all ();
        StringV str_val  (* 出力した文字列を返す *)
      | _ -> err "Argument must be a string for print_string")
  | BinOp (op, exp1, exp2) ->
  (match op with
   | And ->
       let val1 = eval_exp env exp1 in
       (match val1 with
        | BoolV false -> BoolV false  (* 短絡評価 *)
        | BoolV true -> 
            let val2 = eval_exp env exp2 in
            (match val2 with
             | BoolV b -> BoolV b
             | _ -> err ("Second argument of && must be boolean"))
        | _ -> err ("First argument of && must be boolean"))
   | Or ->
       let val1 = eval_exp env exp1 in
       (match val1 with
        | BoolV true -> BoolV true   (* 短絡評価 *)
        | BoolV false -> 
            let val2 = eval_exp env exp2 in
            (match val2 with
             | BoolV b -> BoolV b
             | _ -> err ("Second argument of || must be boolean"))
        | _ -> err ("First argument of || must be boolean"))
   | _ ->  (* その他の演算子は従来通り *)
       let arg1 = eval_exp env exp1 in
       let arg2 = eval_exp env exp2 in
       apply_prim op arg1 arg2)
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
     (* 現在の環境で exp1 を評価 *)
    let value = eval_exp env exp1 in
     (* exp1 の評価結果を id の値として環境に追加して exp2 を評価 *)
    eval_exp (Environment.extend id value env) exp2
  (*3.4.2*)
  | OpExp op -> OpV op (* 新規追加：演算子を値として扱う *)
   (* 関数定義式: 現在の環境 env をクロージャ内に保存 *)
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  (* 3.4.5　動的束縛：環境を保存しない *)
  | DFunExp (id, exp) -> DProcV (id, exp) 
  (* 関数適用式 *)
  | AppExp (exp1, exp2) ->
      (* 関数 exp1 を現在の環境で評価 *)
      let funval = eval_exp env exp1 in
      (* 実引数 exp2 を現在の環境で評価 *)
      let arg = eval_exp env exp2 in
      (* 関数 exp1 の評価結果をパターンマッチで取り出す *)
      (match funval with
          ProcV (id, body, env') -> (* 評価結果が実際にクロージャであれば *)
              (* クロージャ内の環境を取り出して仮引数に対する束縛で拡張 *)
              let newenv = Environment.extend id arg  (!env') in
                eval_exp newenv body
          | OpV op ->                 (* 新規追加：演算子の部分適用 *)
              (* 演算子の第一引数を受け取り、第二引数を待つ関数を返す *)
              ProcV ("y", 
                     BinOp (op, 
                            (match arg with
                             | IntV i -> ILit i
                             | BoolV b -> BLit b
                             | _ -> err "Invalid argument for operator"),
                            Var "y"),
                     ref env)
          | DProcV (id, body) ->                          (* 動的束縛：現在の環境を使用 *)
              let newenv = Environment.extend id arg env in
              eval_exp newenv body
          | _ ->
          (* 評価結果がクロージャでなければ，実行時型エラー *)
          err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) ->
    let dummyenv = ref Environment.empty in
    let func_closure = ProcV (para, exp1, dummyenv) in
    let newenv = Environment.extend id func_closure env in
    dummyenv := newenv;  (* クロージャの環境を正しく設定 *)
    eval_exp newenv exp2


let eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) ->
      let v = eval_exp env e in (id, Environment.extend id v env, v)
  | RecDecl (id, param, body) ->
    let dummyenv = ref Environment.empty in
    let func_closure = ProcV (param, body, dummyenv) in
    let newenv = Environment.extend id func_closure env in
    dummyenv := newenv;
    (id, newenv, func_closure)  (* 同じクロージャを返す *)