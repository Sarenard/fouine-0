open Expr
open Exceptions

let new_uvar (vars : (ty list ref)) : ty =
  let newvar = Tuvar (ref None) in
  vars := newvar :: !vars;
  newvar
;;

let rec infer (env : infer_env) (vars: (ty list ref)) (expr: expr) : ty = 
  match expr with
  | Int _ -> Tint
  | App (t1, t2) ->
    let v1 = new_uvar vars in
    let v2 = new_uvar vars in
    unify (infer env vars t1) (Tarr (v1, v2));
    unify (infer env vars t2) v1;
    v2
  | String var_name -> (
    match List.assoc_opt var_name env with
    | None -> raise (UnknownVariable var_name);
    | Some t -> t
  )
  | Fun (PVar x, body) ->
    let newvar = new_uvar vars in
    let newenv = (x, newvar)::env in
    let bodyty = infer newenv vars body in
    Tarr (newvar, bodyty)
  | _ -> 
    raise UnimplementedError;

(*Implémente l'unification de deux termes*)
and unify (t1: ty) (t2 : ty) : unit =
  let nt1, nt2 = canonic t1, canonic t2 in
  match (nt1, nt2) with
  | Tint, Tint -> ()
  | Tarr (a, b), Tarr (c, d) ->
    unify a c;
    unify b d;
  | Tuvar r1, Tuvar r2 when r1 == r2 -> ()
  | Tuvar r, t ->
    r := Some (canonic t);
  | t, Tuvar r -> 
    unify (Tuvar r) t
  | _ -> raise Not_unifyable
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
