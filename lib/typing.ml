open Expr
open Exceptions

let uvar_number = ref 0

let new_uvar = fun () ->
  let v = "X"^(string_of_int !uvar_number) in
  (incr uvar_number; v);;

let rec pattern_to_ty (pat: pattern) : (ty * (var * (var list * ty)) list) =
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

let rec infer (env : (var * (var list * ty)) list) (v : var) (t: expr) : unif_pbm = match t with
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
  (*not 100% sure is correct*)
  | Let (pattern, t1, t2, false) ->
    let a1 = new_uvar () in
    let u1 = infer env a1 t1 in
    let (ptype, bindings) = pattern_to_ty pattern in
    let env' = bindings @ env in
    let u2 = infer env' v t2 in
    (Tuvar a1, ptype) :: u1 @ u2
  | If (e1, e2, e3) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let a3 = new_uvar () in
    let u1 = infer env a1 e1 in
    let u2 = infer env a2 e2 in
    let u3 = infer env a3 e3 in
    [(Tuvar a1, Tbool); (Tuvar v, Tuvar a2); (Tuvar a2, Tuvar a3)]@u1@u2@u3
  | expr -> 
    print_string "Erreur, expr inconnue dans le typage :\n";
    affiche_expr expr;
    print_string "\n";
    raise UnimplementedError;;

(*Indique si une variable apparait dans ce qui existe déjà*)
let rec appear (x : var) (term : ty) : bool =
  match term with
  | Tint | Tbool | Tpolyvar _ -> false
  | Tuvar y -> x = y
  | Tref t -> appear x t
  | Tprod lst -> List.exists (appear x) lst
  | Tarr(t1, t2) -> appear x t1 || appear x t2

(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace ((x, new_x) : var * ty) (term : ty) : ty =
  match term with
  | Tint | Tbool | Tpolyvar _ -> term
  | Tuvar y when y = x -> new_x
  | Tuvar _ -> term
  | Tref t -> Tref (replace (x, new_x) t);
  | Tprod lst -> Tprod (List.map (replace (x, new_x)) lst)
  | Tarr(t1, t2) -> Tarr(
    replace (x, new_x) t1,
    replace (x, new_x) t2
  );;

let apply_subst (sb : subst) (term : ty) : ty =
  List.fold_left (fun acc (x, u) -> replace (x, u) acc) term sb

(*Implémente l'unification de deux termes*)
let unify (pb : unif_pbm) : subst =
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
        Some sub
      with Not_unifyable ->
        Printf.printf "Not Unifyable.\n";
        Printf.printf "Constraints (pbm):\n";
        print_pbm pbm;
        Printf.printf "\n";
        raise Not_unifyable
    with e ->
      Printf.printf "Error : uncaught exception '%s'.\n\n" (Printexc.to_string e);
      None
  end

let main (expression : Expr.expr) (debug: bool) : subst option = 
  typer expression debug
;;
