open Expr
open Exceptions

let uvar_number = ref 0

let new_uvar = fun () ->
  let v = "X"^(string_of_int !uvar_number) in
  (incr uvar_number; v);;

let rec infer (env : (var * ty) list) (v : var) (t: expr) : unif_pbm = match t with
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
  | Int _ -> [(Tuvar v, Tint)]
  | Bool _ -> [(Tuvar v, Tbool)]
  | Tuple exprlst -> 
    let vars = List.map (fun _ -> new_uvar ()) exprlst in
    let cs_elems =
      List.concat (List.map2 (fun vi ei -> infer env vi ei) vars exprlst)
    in
    (Tuvar v, Tprod (List.map (fun vi -> Tuvar vi) vars)) :: cs_elems
  | String v1 -> let tv =
      try List.assoc v1 env
      with Not_found -> raise Not_inferable;
    in
    [ (Tuvar v, tv) ]
  | Fun (PVar x, body) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let env' = (x, Tuvar a1) :: env in
    let u = infer env' a2 body in
    (Tuvar v, Tarr (Tuvar a1, Tuvar a2)) :: u
  | App (t1, t2) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let u1 = infer env a1 t1 in
    let u2 = infer env a2 t2 in
    (Tuvar a1, Tarr (Tuvar a2, Tuvar v)) :: (u1 @ u2)
  | Let (PVar x, t1, t2, false) ->
    let a1 = new_uvar () in
    let u1 = infer env a1 t1 in
    let env' = (x, Tuvar a1) :: env in
    let u2 = infer env' v t2 in
    u1 @ u2
  | If (e1, e2, e3) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let a3 = new_uvar () in
    let u1 = infer env a1 e1 in
    let u2 = infer env a2 e2 in
    let u3 = infer env a3 e3 in
    [(Tuvar a1, Tbool); (Tuvar v, Tuvar a2); (Tuvar a2, Tuvar a3)]@u1@u2@u3
  | _ -> raise UnimplementedError;;

(*Indique si une variable apparait dans ce qui existe déjà*)
let rec appear (x : var) (term : ty) : bool =
  match term with
  | Tint | Tbool -> false
  | Tuvar y -> x = y
  | Tprod lst -> List.exists (appear x) lst
  | Tarr(t1, t2) -> appear x t1 || appear x t2

(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace ((x, new_x) : var * ty) (term : ty) : ty =
  match term with
  | Tint | Tbool -> term
  | Tuvar y when y = x -> new_x
  | Tuvar _ -> term
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

        | _ ->
            raise Not_unifyable
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
