(* un type pour des expressions simples *)
type expr =
    Int of int
  | Bool of bool
  | String of string
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | Or of expr*expr
  | And of expr*expr
  | If of expr*expr*expr
  | PrInt of expr
  | Let of string*expr*expr

type env = (string * expr) list

type valeur = 
    VI of int
  | VB of bool
  | Boom
  
let print_bool = function
  | true  -> print_string "true"
  | false -> print_string "false"

(* fonction d'affichage *)
let rec affiche_expr e =
  let aff_aux s a b = 
      begin
        print_string s;
        affiche_expr a;
        print_string ", ";
        affiche_expr b;
        print_string ")"
      end
  in
  match e with
  | Int k -> print_int k
  | Bool k -> print_bool k
  | String k -> print_string k
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Or(e1,e2) -> (
	  print_string "Or(";
    affiche_expr e1;
	  print_string " || ";
    affiche_expr e2;
    print_string ")";
    )
  | And(e1,e2) -> (
      print_string "And(";
      affiche_expr e1;
      print_string " && ";
      affiche_expr e2;
      print_string ")";
  )
  | If(e1, e2, e3) ->
    (print_string "If(";
    affiche_expr e1;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ")";)
  | Let(e1, e2, e3) ->
    (print_string "Let(";
    print_string e1;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ")";)
  | PrInt(e) -> (
    print_string "PrInt(";
    affiche_expr e;
    print_string ")";
  )

let print_env env = List.iter (fun (x, e) ->
      print_string x; print_string " -> "; affiche_expr e; print_newline ();
    ) env;;

let affiche_val v = 
  match v with 
  | VI k -> print_int k
  | VB k -> print_bool k
  | Boom -> print_string "Boom"

let prInt x = print_int x;print_newline(); x;;

(* sémantique opérationnelle à grands pas *)

let affiche_valeur v =
  match v with 
  | VI k -> (print_int k; print_newline ())
  | VB k -> (print_bool k; print_newline ())
  | Boom -> (print_string "Boom"; print_newline ())

(* évaluation d'une expression en une valeur *)
let rec eval value env = match value with 
  | Int k -> VI k
  | Bool b -> VB b
  | String s -> (match List.assoc_opt s env with 
    | Some e1 -> eval e1 env
    | None -> Boom)
  | Add(e1,e2) ->
    let v1, v2 = eval e1 env, eval e2 env in (
      match (v1,v2) with
      | (VI k1,VI k2) -> VI (k1+k2)
      | _ -> Boom
    )
  | Mul(e1,e2) -> 
    let v1, v2 = eval e1 env, eval e2 env in (
      match (v1,v2) with
      | (VI k1,VI k2) -> VI (k1 * k2)
      | _ -> Boom
    )
  | Min(e1,e2) -> 
    let v1, v2 = eval e1 env, eval e2 env in (
      match (v1,v2) with
      | (VI k1,VI k2) -> VI (k1 - k2)
      | _ -> Boom
    )
  | If(e1, e2, e3) -> 
    let v = eval e1 env in (
      match v with
      | (VB true) -> eval e2 env
      | (VB false) -> eval e3 env
      | _ -> Boom
    )
  | Or(e1,e2) -> 
    let v1, v2 = eval e1 env, eval e2 env in (
      match (v1,v2) with
      | (VB k1,VB k2) -> VB (k1 || k2)
      | _ -> Boom
    )
  | And(e1,e2) -> 
    let v1, v2 = eval e1 env, eval e2 env in (
      match (v1,v2) with
      | (VB k1,VB k2) -> VB (k1 && k2)
      | _ -> Boom
    )
  | PrInt(e) -> 
    let v = eval e env in (
      match v with 
      | (VI k) -> (VI (prInt k));
      | _ -> Boom
    )
  | Let(str, e1, e2) -> eval e2 ((str, e1)::env);
