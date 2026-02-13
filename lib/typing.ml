open Expr
open Exceptions

let uvar_number = ref 0

let new_uvar = fun () ->
  let v = "X"^(string_of_int !uvar_number) in
  (incr uvar_number; v);;

let rec pattern_to_ty (pat: pattern) : (ty * infer_env) (*le type du pattern et les bindings*)=
  match pat with
  | PBool _ -> (Tbool, [])
  | PInt _ -> (Tint, [])
  | PVar var -> 
    let nv = Tuvar (new_uvar ()) in
    (nv, [(var, ([], nv))])
  | PTuple lst -> 
    let mlist = List.map (fun pat -> pattern_to_ty pat) lst in
    (
      Tprod (List.map fst mlist),
      List.concat (List.map snd mlist)
    )
;;

let rec infer (env : infer_env) (v : var) (t: expr) : unif_pbm = match t with
| Op(op, t1, t2) -> (
    let x1 = new_uvar () in let x2 = new_uvar () in
    let u1 = infer env x1 t1 in let u2 = infer env x2 t2 in
    match op with
    | "+" | "-" | "/" | "*" ->
      [(Tuvar v, Tint); (Tuvar x1, Tint); (Tuvar x2, Tint)]@u1@u2
    | "<" | "<=" | ">" | ">=" ->
      [(Tuvar v, Tbool); (Tuvar x1, Tint); (Tuvar x2, Tint)]@u1@u2
    | "&&" | "||" ->
      [(Tuvar v, Tbool); (Tuvar x1, Tbool); (Tuvar x2, Tbool)]@u1@u2
    | "=" | "<>" -> 
      [(Tuvar v, Tbool); (Tuvar x1, Tuvar x2)]@u1@u2
    | _ -> raise UnimplementedError
  )
  | Seq(e1, e2) ->
    let x1 = new_uvar () in let x2 = new_uvar () in
    let u1 = infer env x1 e1 in let u2 = infer env x2 e2 in
    [(Tuvar x1, Tprod []); (Tuvar v, Tuvar x2)]@u1@u2
  | Int _ -> [(Tuvar v, Tint)]
  | Bool _ -> [(Tuvar v, Tbool)]
  | Tuple exprlst -> 
    let vars = List.map (fun _ -> new_uvar ()) exprlst in
    let cs_elems =
      List.concat (List.map2 (fun vi ei -> infer env vi ei) vars exprlst)
    in
    (Tuvar v, Tprod (List.map (fun vi -> Tuvar vi) vars)) :: cs_elems
  | String v1 -> (
      match List.assoc_opt v1 env with
      | None -> raise Not_inferable;
      | Some (lstargs, rty) -> 
        let sb = List.map (fun polyvar -> (polyvar, Tuvar (new_uvar ()))) lstargs in
        let newty = replace_polyvar sb rty in
        [ (Tuvar v, newty) ]
    )
  | App (t1, t2) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let u1 = infer env a1 t1 in
    let u2 = infer env a2 t2 in
    (Tuvar a1, Tarr (Tuvar a2, Tuvar v)) :: (u1 @ u2)
  | Fun (pattern, body) ->
    let (ptype, bindings) = pattern_to_ty pattern in
    let env' = bindings @ env in
    let a2 = new_uvar () in
    let u = infer env' a2 body in
    (Tuvar v, Tarr (ptype, Tuvar a2)) :: u
  (*No generalisation version !!*)
  | Let (pattern, t1, t2, false) (*not rec*) ->
    let (ptype, bindings) = pattern_to_ty pattern in
    let a1 = new_uvar () in
    let u1 = infer env a1 t1 in
    let env2 = bindings @ env in
    let u2 = infer env2 v t2 in
    (Tuvar a1, ptype) :: u1 @ u2
  (* 
  (*Generalisation version !!*)
  | Let (pattern, _t1, _t2, false) (*not rec*) ->
    let (_ptype, bindings) = pattern_to_ty pattern in
    let _bindings_types = List.map (
      fun x -> x
    ) bindings in
    raise UnimplementedError;
  *)

  | Let (pattern, t1, t2, true) (*rec*) -> (
    match pattern with
    | PVar s -> (
        let a1 = new_uvar () in
        let env2 = (s, ([], Tuvar a1))::env in
        let u1 = infer env2 a1 t1 in
        let u2 = infer env2 v t2 in
        u1 @ u2
      )
    | _ -> raise Unreachable
  )
  | If (e1, e2, e3) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let a3 = new_uvar () in
    let u1 = infer env a1 e1 in
    let u2 = infer env a2 e2 in
    let u3 = infer env a3 e3 in
    [(Tuvar a1, Tbool); (Tuvar v, Tuvar a2); (Tuvar a2, Tuvar a3)]@u1@u2@u3
  | Match (expr, lst) ->
    let a1 = new_uvar () in
    let u1 = infer env a1 expr in
    let constraints = List.concat_map (
      fun (pat, exp) ->
        let (ptype, bindings) = pattern_to_ty pat in
        let env' = bindings @ env in
        let newvar = new_uvar () in
        let u2 = infer env' v exp in
        (Tuvar a1, ptype) :: (Tuvar newvar, Tuvar v) :: u2
    ) lst in constraints @ u1

(*Implémente l'unification de deux termes*)
and unify (pb : unif_pbm) : subst =
  let rec unify_aux (pb : unif_pbm) (sb : subst) : subst =
    match pb with
    | [] -> sb
    | (t1, t2) :: rest ->
      let t1 = apply_subst sb t1 in
      let t2 = apply_subst sb t2 in
      if t1 = t2 then unify_aux rest sb else
        match (t1, t2) with
        | (Tint, Tint) | (Tbool, Tbool) ->
            unify_aux rest sb
          
        | (Tprod l1, Tprod l2) when List.length l1 = List.length l2 ->
            unify_aux (List.combine l1 l2 @ rest) sb

        | (Tref a, Tref b) ->
          unify_aux ((a, b)::rest) sb

        | (Tarr (a1, b1), Tarr (a2, b2)) ->
            unify_aux ((a1, a2) :: (b1, b2) :: rest) sb

        | (Tuvar x, ty) ->
            if appear x ty then raise Not_unifyable;
            let rest' =
              List.map (fun (a, b) -> (replace (x, ty) a, replace (x, ty) b)) rest
            in
            let sb' =
              List.map (fun (y, t_y) -> (y, replace (x, ty) t_y)) sb
            in
            unify_aux rest' ((x, ty) :: sb')

        | (ty, Tuvar x) ->
            unify_aux ((Tuvar x, ty) :: rest) sb

        | (t1, t2) -> 
          Printf.printf "Unify_aux err : (%s = %s)\n" (string_of_ty t1) (string_of_ty t2);
          raise Not_unifyable;
  in
  unify_aux pb []

let typer (t : expr) (debug: bool) =
  begin
    if debug then (
      print_string "# inférence sur "; affiche_expr t; print_string "\n";
    );
    try
      let v0 = new_uvar () in
      let pbm = infer empty_env_type v0 t in
      try
        let sub = unify pbm in
        if debug then (
          Printf.printf "Solution : \n";
          print_sub sub;
        );
        let ty_v0 = apply_subst sub (Tuvar v0) in
        if debug then (
          Printf.printf "Type inféré : %s\n\n" (string_of_ty ty_v0);
        );
        sub
      with Not_unifyable ->
        Printf.printf "Not Unifyable.\n";
        Printf.printf "Constraints (pbm):\n";
        print_pbm pbm;
        Printf.printf "\n";
        raise Not_unifyable
    with e ->
      Printf.printf "Error : uncaught exception '%s'.\n\n" (Printexc.to_string e);
      raise Not_unifyable
  end

let main (expression : Expr.expr) (debug: bool) : subst = 
  typer expression debug
;;
