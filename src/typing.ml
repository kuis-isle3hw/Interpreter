open Syntax
open Eval

(* Exercise 4.3.2: 型代入（substitution）関数 *)
(* 型変数に具体的な型を代入して型を変換する *)
type subst = (tyvar * ty) list

let subst_type (s : subst) (ty : ty) : ty =
  let rec apply_subst (var, ty_subst) ty_target =
    match ty_target with
    | TyInt 
    | TyBool -> ty_target
    | TyString -> TyString  (* 文字列型の追加 *)
    | TyVar id ->
        if id = var then ty_subst
        else TyVar id
    | TyFun (ty1, ty2) ->
        TyFun (apply_subst (var, ty_subst) ty1, apply_subst (var, ty_subst) ty2)
    | TyList t ->
        TyList (apply_subst (var, ty_subst) t)
  in
  List.fold_left (fun acc_ty subst_pair -> apply_subst subst_pair acc_ty) ty s

(* 型代入の合成 *)
let compose_subst s1 s2 =
  let apply_s1_to_s2 = List.map (fun (tv, ty) -> (tv, subst_type s1 ty)) s2 in
  let s1_vars = List.map fst s1 in
  let s2_without_s1_vars = List.filter (fun (tv, _) -> not (List.mem tv s1_vars)) apply_s1_to_s2 in
  s1 @ s2_without_s1_vars

(* Exercise 4.3.3: 単一化（unification）アルゴリズム *)
(* 型の等式を解いて型変数への代入を求める *)
let rec unify (eqs : (ty * ty) list) : subst =
  match eqs with
  | [] -> []  (* 制約なし：空の代入 *)
  
  (* 同じ基本型同士：制約削除 *)
  | (TyInt, TyInt) :: rest 
  | (TyBool, TyBool) :: rest ->
      unify rest
  | (TyString, TyString) :: rest ->  (* 文字列型の追加 *)
      unify rest
  
  (* 型変数と基本型の等式：型変数に基本型を代入 *)
  (* 型変数と型の等式（対称性を考慮） *)
  | (TyVar id, ty) :: rest 
  | (ty, TyVar id) :: rest ->
      if ty = TyVar id then 
        (* 同じ型変数：制約削除 *)
        unify rest  
      else begin
        (* オカーチェック：型変数が自分自身を含む型に代入されることを防ぐ *)
        if MySet.member id (freevar_ty ty) then 
          raise (Error "occurs check failed")
        else
          (* 型変数idを型tyで置換 *)
          let subst_single = [(id, ty)] in
          let rest' = List.map (fun (l, r) -> 
            (subst_type subst_single l, subst_type subst_single r)) rest in
          (* 代入を記録して残りを処理 *)
          subst_single @ (unify rest')
      end
  
  (* 関数型同士の等式：構造的分解 *)
  | (TyFun (a1, a2), TyFun (b1, b2)) :: rest ->
      (* 引数型と戻り値型の等式に分解 *)
      unify ((a1, b1) :: (a2, b2) :: rest)
  
  (* リスト型同士の等式：構造的分解 *)
  | (TyList ty1, TyList ty2) :: rest ->
      (* 要素型の等式に分解 *)
      unify ((ty1, ty2) :: rest)
  
  (* その他の場合：単一化不可能 *)
  | _ -> raise (Error "unification failed")

exception Error of string

let err s = raise (Error s)

(* 型環境：識別子から型への写像 *)
type tyenv = ty Environment.t

(* 型変数生成用のカウンター（typing.ml内で独立管理） *)
let tyvar_counter = ref 0

let fresh_tyvar () =
 let n = !tyvar_counter in
 tyvar_counter := n + 1;
 n

(* 型代入を等式制約の集合に変換 *)
let eqs_of_subst subst =
  List.map (fun (tyvar, ty) -> (TyVar tyvar, ty)) subst

(* プリミティブ演算の型制約を生成（型推論版） *)
let ty_prim op ty1 ty2 = 
  match op with
  | Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

(* 型推論システム：式の型推論（型代入と型のペアを返す） *)
let rec ty_exp tyenv = function
   Var x ->                                    (* 変数 *)
     (try ([], Environment.lookup x tyenv) with
         Environment.Not_bound -> err ("variable not bound: " ^ x))
 | ILit _ -> ([], TyInt)                       (* 整数リテラル *)
 | BLit _ -> ([], TyBool)                      (* 論理値リテラル *)
 | SLit _ -> ([], TyString)                   (* 文字列リテラル *)
 | StrConcatExp (exp1, exp2) -> (* 文字列連結式 *)
     let (s1, ty1) = ty_exp tyenv exp1 in
     let (s2, ty2) = ty_exp tyenv exp2 in
     let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyString); (ty2, TyString)] in
     let s_final = unify eqs in
     (s_final, TyString) (* 文字列型を返す *)
 | StrGetExp (exp1, exp2) -> (* 文字列インデックス取得 *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyString); (ty2, TyInt)] in
      let s_final = unify eqs in
      (s_final, TyString) (* 文字列型を返す *)
 | PrintStrExp exp -> (* 文字列出力 *)
     let (s, ty) = ty_exp tyenv exp in
     let ty_res = TyVar (fresh_tyvar ()) in
     let eqs = (eqs_of_subst s) @ [(ty, TyString)] in
     let s_final = unify eqs in
     (s_final, TyString)
 | BinOp (op, exp1, exp2) ->                   (* 二項演算 *)
     let (s1, ty1) = ty_exp tyenv exp1 in
     let tyenv1 = Environment.map (subst_type s1) tyenv in
     let (s2, ty2) = ty_exp tyenv1 exp2 in
     let (eqs, ty) = ty_prim op (subst_type s2 ty1) ty2 in
     let s3 = unify eqs in
     let s = compose_subst s3 (compose_subst s2 s1) in
     (s, subst_type s3 ty)
 | IfExp (exp1, exp2, exp3) ->                 (* 条件式 *)
     let (s1, ty1) = ty_exp tyenv exp1 in
     let tyenv1 = Environment.map (subst_type s1) tyenv in
     let (s2, ty2) = ty_exp tyenv1 exp2 in
     let tyenv2 = Environment.map (subst_type s2) tyenv1 in
     let (s3, ty3) = ty_exp tyenv2 exp3 in
     let s = compose_subst s3 (compose_subst s2 s1) in
     let eqs = [(subst_type s ty1, TyBool); (subst_type s ty2, subst_type s ty3)] in
     let s4 = unify eqs in
     let final_s = compose_subst s4 s in
     (final_s, subst_type s4 (subst_type s ty2))
 | LetExp (id, exp1, exp2) ->                  
     let (s1, ty1) = ty_exp tyenv exp1 in
     let tyenv1 = Environment.extend id ty1 (Environment.map (subst_type s1) tyenv) in
     let (s2, ty2) = ty_exp tyenv1 exp2 in
     let s = compose_subst s2 s1 in
     (s, ty2)
 | FunExp (id, exp) ->                         
     let tyvar = TyVar (fresh_tyvar ()) in     (* 引数の型変数を生成 *)
     let tyenv1 = Environment.extend id tyvar tyenv in
     let (s, ty) = ty_exp tyenv1 exp in
     (s, TyFun (subst_type s tyvar, ty))       (* 関数型を構築 *)
 | DFunExp (id, exp) ->                        (* Exercise 3.4.5: 動的束縛関数 *)
     let tyvar = TyVar (fresh_tyvar ()) in     (* 型推論は静的束縛と同じ *)
     let tyenv1 = Environment.extend id tyvar tyenv in
     let (s, ty) = ty_exp tyenv1 exp in
     (s, TyFun (subst_type s tyvar, ty))       (* 型レベルでは静的束縛と区別しない *)
 | AppExp (exp1, exp2) ->                      (* Exercise 3.3: 関数適用 *)
     let (s1, ty1) = ty_exp tyenv exp1 in
     let tyenv1 = Environment.map (subst_type s1) tyenv in
     let (s2, ty2) = ty_exp tyenv1 exp2 in
     let tyvar = TyVar (fresh_tyvar ()) in
     let eqs = [(subst_type s2 ty1, TyFun (ty2, tyvar))] in
     let s3 = unify eqs in
     let s = compose_subst s3 (compose_subst s2 s1) in
     (s, subst_type s3 tyvar)
 | OpExp op ->                                 (* Exercise 3.4.2: 演算子値の型チェック *)
     (* 演算子を第一級関数として扱うための型を返す（カリー化された関数型） *)
     (match op with
      | Plus -> ([], TyFun (TyInt, TyFun (TyInt, TyInt)))      (* (+) : int -> int -> int *)
      | Mult -> ([], TyFun (TyInt, TyFun (TyInt, TyInt)))      (* (**) : int -> int -> int *)
      | Lt -> ([], TyFun (TyInt, TyFun (TyInt, TyBool)))       (* (<) : int -> int -> bool *)
      | And -> ([], TyFun (TyBool, TyFun (TyBool, TyBool)))    (* (&&) : bool -> bool -> bool *)
      | Or -> ([], TyFun (TyBool, TyFun (TyBool, TyBool))))    (* (||) : bool -> bool -> bool *)
 | LetRecExp (f, x, exp1, exp2) ->             
     let tyvar_f = TyVar (fresh_tyvar ()) in   (* 関数名の型変数 *)
     let tyvar_x = TyVar (fresh_tyvar ()) in   (* 引数の型変数 *)
     let tyenv1 = Environment.extend f tyvar_f (Environment.extend x tyvar_x tyenv) in
     let (s1, ty1) = ty_exp tyenv1 exp1 in     (* 関数本体の型をチェック *)
     let fun_ty = TyFun (subst_type s1 tyvar_x, ty1) in    (* 実際の関数型を構築 *)
     let s2 = unify [(subst_type s1 tyvar_f, fun_ty)] in
     let s = compose_subst s2 s1 in
     let final_fun_ty = subst_type s fun_ty in
     let tyenv2 = Environment.extend f final_fun_ty (Environment.map (subst_type s) tyenv) in
     let (s3, ty2) = ty_exp tyenv2 exp2 in
     let final_s = compose_subst s3 s in
     (final_s, ty2)

(* Exercise 4.3.5: 宣言の型推論 *)
let ty_decl tyenv = function
   Exp e ->                                    (* 式の評価 *)
     let (s, ty) = ty_exp tyenv e in
     let final_ty = subst_type s ty in
     (final_ty, tyenv)                         (* cui.ml互換：型と環境のペアを返す *)
 | Decl (id, e) ->                             (* 変数宣言 *)
     let (s, ty) = ty_exp tyenv e in
     let final_ty = subst_type s ty in
     let new_tyenv = Environment.extend id final_ty (Environment.map (subst_type s) tyenv) in
       (final_ty, new_tyenv)                   (* 拡張された環境を返す *)
 | RecDecl (f, x, e) ->                        
     let tyvar_f = TyVar (fresh_tyvar ()) in
     let tyvar_x = TyVar (fresh_tyvar ()) in
     let tyenv1 = Environment.extend f tyvar_f (Environment.extend x tyvar_x tyenv) in
     let (s1, ty1) = ty_exp tyenv1 e in
     let fun_ty = TyFun (subst_type s1 tyvar_x, ty1) in
     let s2 = unify [(subst_type s1 tyvar_f, fun_ty)] in
     let s = compose_subst s2 s1 in
     let final_fun_ty = subst_type s fun_ty in
     let new_tyenv = Environment.extend f final_fun_ty (Environment.map (subst_type s) tyenv) in
     (final_fun_ty, new_tyenv)