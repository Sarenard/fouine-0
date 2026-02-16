open Expr
open Exceptions

let new_uvar (vars : (ty list ref)) : ty =
  let newvar = Tuvar (ref None) in
  vars := newvar :: !vars;
  newvar
;;

let rec pattern_to_ty (pat: pattern) (vars : (ty list ref)) : (ty * infer_env) (*le type du pattern et les bindings*)=
  match pat with
  | PBool _ -> (Tbool, [])
  | PInt _ -> (Tint, [])
  | PVar var -> 
    let nv = new_uvar vars in
    (nv, [(var, ([], nv))])
  | PTuple lst -> 
    let mlist = List.map (fun pat -> pattern_to_ty pat vars) lst in
    (
      Tprod (List.map fst mlist),
      List.concat (List.map snd mlist)
    )
;;

let rec infer (env : infer_env) (vars: (ty list ref)) (expr: expr) : ty = 
  match expr with
  | Op(op, t1, t2) -> (
    let x1 = new_uvar vars in let x2 = new_uvar vars in
    let u1 = infer env vars t1 in let u2 = infer env vars t2 in
    match op with
    | "+" | "-" | "/" | "*" ->
      unify u1 Tint; unify u2 Tint; Tint
    | "<" | "<=" | ">" | ">=" ->
      unify u1 Tint; unify u2 Tint; Tbool
    | "&&" | "||" ->
      unify u1 Tbool; unify u2 Tbool; Tbool
    | "=" | "<>" -> 
      unify u1 u2; Tbool
    | _ -> raise UnimplementedError
  )
  | Int _ -> Tint
  | Bool _ -> Tbool
  | Seq (e1, e2) ->
    let e1ty = infer env vars e1 in
    let e2ty = infer env vars e2 in
    unify e1ty (Tprod []);
    e2ty
  | Tuple lst ->
    Tprod (List.map (infer env vars) lst)
  | App (t1, t2) ->
    let v1 = new_uvar vars in
    let v2 = new_uvar vars in
    unify (infer env vars t1) (Tarr (v1, v2));
    unify (infer env vars t2) v1;
    v2
  | String var_name -> (
    match List.assoc_opt var_name env with
    | None -> raise (UnknownVariable var_name);
    | Some (lstargs, rty) -> 
        let sb = List.map (fun polyvar -> (polyvar, new_uvar vars)) lstargs in
        replace_polyvar sb rty
  )
  | Fun (pat, body) ->
    let (patty, bindings) = pattern_to_ty pat vars in
    let newenv = bindings@env in
    let bodyty = infer newenv vars body in
    Tarr (patty, bodyty)
  | Let (pat, e1, e2, false) (*not rec*) ->
    let (patty, bindings) = pattern_to_ty pat vars in
    let newenv = bindings@env in
    let e1ty = infer env vars e1 in
    let e2ty = infer newenv vars e2 in
    unify patty e1ty;
    e2ty
  | Let (pat, e1, e2, true) (*rec*) ->
    let (patty, bindings) = pattern_to_ty pat vars in
    let newenv = bindings@env in
    let e1ty = infer newenv vars e1 in
    let e2ty = infer newenv vars e2 in
    unify patty e1ty;
    e2ty
  | If (cond, e1, e2) ->
    let condty = infer env vars cond in
    let e1ty = infer env vars e1 in
    let e2ty = infer env vars e2 in
    unify condty Tbool;
    unify e1ty e2ty;
    e1ty
  | _ -> 
    raise UnimplementedError;

(*Implémente l'unification de deux termes*)
and unify (t1: ty) (t2 : ty) : unit =
  let nt1, nt2 = canonic t1, canonic t2 in
  match (nt1, nt2) with
  | (Tint, Tint )-> ()
  | (Tbool, Tbool) -> ()
  | (Tarr (a, b), Tarr (c, d)) ->
    unify a c;
    unify b d;
  | (Tprod t1, Tprod t2) ->
    List.iter2 unify t1 t2;
  | (Tref a, Tref b) ->
    unify a b;
  | (Tuvar r1, Tuvar r2) when r1 == r2 ->
    ()
  | (Tuvar r, t) ->
    r := Some (canonic t);
  | (t, Tuvar r) -> 
    unify (Tuvar r) t
  | (t1, t2) -> 
    Printf.printf "Unify_aux err : (%s = %s)\n" (string_of_ty t1) (string_of_ty t2);
    raise Not_unifyable;
;;

let typer (t : expr) (debug: bool) : ty =
  begin
    if debug then (
      print_string "# inférence sur "; affiche_expr t; print_string "\n";
    );
    try
      let vars = ref [] in
      let typ = infer empty_env_type vars t in
      if debug then (
        print_string ("Type :\n  " ^ (string_of_ty typ) ^ "\n");
      );
      typ
    with e ->
      Printf.printf "Error : uncaught exception '%s'.\n\n" (Printexc.to_string e);
      raise Not_unifyable
  end

let main (expression : Expr.expr) (debug: bool) : ty = 
  typer expression debug
;;
