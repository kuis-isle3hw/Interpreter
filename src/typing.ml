open Syntax

(* 型の定義 *)
type ty = 
  | TyInt                    (* 整数型 *)
  | TyString                 (* 文字列型 *)
  | TyPair of ty * ty        (* ペア型 *)
  | TyVar of tyvar           (* 型変数 *)

and tyvar = int

(* 型代入（substitution）関数 *)
type subst = (tyvar * ty) list

let rec subst_type (s : subst) (ty : ty) : ty =
  let rec apply_subst (var, ty_subst) ty_target =
    match ty_target with
    | TyInt | TyString -> ty_target
    | TyVar id ->
        if id = var then ty_subst else TyVar id
    | TyPair (ty1, ty2) ->
        TyPair (apply_subst (var, ty_subst) ty1, apply_subst (var, ty_subst) ty2)
  in
  List.fold_left (fun acc_ty subst_pair -> apply_subst subst_pair acc_ty) ty s

(* 型代入の合成 *)
let compose_subst s1 s2 =
  let apply_s1_to_s2 = List.map (fun (tv, ty) -> (tv, subst_type s1 ty)) s2 in
  let s1_vars = List.map fst s1 in
  let s2_without_s1_vars = List.filter (fun (tv, _) -> not (List.mem tv s1_vars)) apply_s1_to_s2 in
  s1 @ s2_without_s1_vars

(* 型に含まれる自由型変数を取得 *)
let rec freevar_ty = function
  | TyInt | TyString -> []
  | TyVar tv -> [tv]
  | TyPair (ty1, ty2) -> freevar_ty ty1 @ freevar_ty ty2

(* 単一化（unification）アルゴリズム *)
exception Error of string

let rec unify (eqs : (ty * ty) list) : subst =
  match eqs with
  | [] -> []  (* 制約なし：空の代入 *)
  
  (* 同じ基本型同士：制約削除 *)
  | (TyInt, TyInt) :: rest 
  | (TyString, TyString) :: rest ->
      unify rest
  
  (* 型変数と型の等式（対称性を考慮） *)
  | (TyVar id, ty) :: rest 
  | (ty, TyVar id) :: rest ->
      if ty = TyVar id then 
        unify rest  (* 同じ型変数：制約削除 *)
      else begin
        (* オカーチェック：型変数が自分自身を含む型に代入されることを防ぐ *)
        if List.mem id (freevar_ty ty) then 
          raise (Error "occurs check failed")
        else
          (* 型変数idを型tyで置換 *)
          let subst_single = [(id, ty)] in
          let rest' = List.map (fun (l, r) -> 
            (subst_type subst_single l, subst_type subst_single r)) rest in
          subst_single @ (unify rest')
      end
  
  (* ペア型同士の等式：構造的分解 *)
  | (TyPair (a1, a2), TyPair (b1, b2)) :: rest ->
      unify ((a1, b1) :: (a2, b2) :: rest)
  
  (* その他の場合：単一化不可能 *)
  | _ -> raise (Error "unification failed")

(* 型環境：識別子から型への写像 *)
type tyenv = ty Environment.t

(* 型変数生成用のカウンター *)
let tyvar_counter = ref 0

let fresh_tyvar () =
  let n = !tyvar_counter in
  tyvar_counter := n + 1;
  n

(* 型代入を等式制約の集合に変換 *)
let eqs_of_subst subst =
  List.map (fun (tyvar, ty) -> (TyVar tyvar, ty)) subst

(* 型推論システム：式の型推論（型代入と型のペアを返す） *)
let rec ty_exp tyenv = function
  | Var x ->                                    (* 変数 *)
      (try ([], Environment.lookup x tyenv) with
          Environment.Not_bound -> raise (Error ("variable not bound: " ^ x)))
  | ILit _ -> ([], TyInt)                       (* 整数リテラル *)
  | SLit _ -> ([], TyString)                    (* 文字列リテラル *)
  
  (* 文字列操作 *)
  | StrConcatExp (exp1, exp2) ->                (* 文字列連結式 *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tyenv1 = Environment.map (subst_type s1) tyenv in
      let (s2, ty2) = ty_exp tyenv1 exp2 in
      let eqs = [(subst_type s2 ty1, TyString); (ty2, TyString)] in
      let s3 = unify eqs in
      let s = compose_subst s3 (compose_subst s2 s1) in
      (s, TyString)
  
  | StrGetExp (exp1, exp2) ->                   (* 文字列インデックス取得 *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tyenv1 = Environment.map (subst_type s1) tyenv in
      let (s2, ty2) = ty_exp tyenv1 exp2 in
      let eqs = [(subst_type s2 ty1, TyString); (ty2, TyInt)] in
      let s3 = unify eqs in
      let s = compose_subst s3 (compose_subst s2 s1) in
      (s, TyString)
  
  | PrintStringExp exp ->                       (* 文字列出力 *)
      let (s, ty) = ty_exp tyenv exp in
      let eqs = [(ty, TyString)] in
      let s_final = unify ((eqs_of_subst s) @ eqs) in
      (s_final, TyString)
  
  (* ペア操作 *)
  | PairExp (exp1, exp2) ->                     (* ペア作成 *)
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tyenv1 = Environment.map (subst_type s1) tyenv in
      let (s2, ty2) = ty_exp tyenv1 exp2 in
      let s = compose_subst s2 s1 in
      (s, TyPair (subst_type s2 ty1, ty2))
  
  | PairProj1Exp exp ->                         (* ペアの第一要素取得 *)
      let (s, ty) = ty_exp tyenv exp in
      let tyvar1 = TyVar (fresh_tyvar ()) in
      let tyvar2 = TyVar (fresh_tyvar ()) in
      let eqs = [(ty, TyPair (tyvar1, tyvar2))] in
      let s_final = unify ((eqs_of_subst s) @ eqs) in
      (s_final, subst_type s_final tyvar1)
  
  | PairProj2Exp exp ->                         (* ペアの第二要素取得 *)
      let (s, ty) = ty_exp tyenv exp in
      let tyvar1 = TyVar (fresh_tyvar ()) in
      let tyvar2 = TyVar (fresh_tyvar ()) in
      let eqs = [(ty, TyPair (tyvar1, tyvar2))] in
      let s_final = unify ((eqs_of_subst s) @ eqs) in
      (s_final, subst_type s_final tyvar2)

(* 宣言の型推論 *)
let ty_decl tyenv = function
  | Exp e ->                                    (* 式の評価 *)
      let (s, ty) = ty_exp tyenv e in
      let final_ty = subst_type s ty in
      (final_ty, tyenv)
  | Decl (id, e) ->                             (* 変数宣言 *)
      let (s, ty) = ty_exp tyenv e in
      let final_ty = subst_type s ty in
      let new_tyenv = Environment.extend id final_ty (Environment.map (subst_type s) tyenv) in
      (final_ty, new_tyenv)

(* 型の文字列表現 *)
let rec string_of_ty = function
  | TyInt -> "int"
  | TyString -> "string"
  | TyPair (ty1, ty2) -> "(" ^ string_of_ty ty1 ^ " * " ^ string_of_ty ty2 ^ ")"
  | TyVar n -> "'" ^ string_of_int n