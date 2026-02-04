open Util

(*expressions*)
type expr =
  | Unit
  | Int of int
  | Bool of bool
  | String of string
  | If of expr*expr*expr
  | Let of string*expr*expr*bool (*true = recursive*)
  | Fun of string*expr
  | App of expr*expr
  | Tuple of expr list
  | Op of string*expr*expr

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
  | App(e1,e2) -> aff_aux "App(" e1 e2
  | Op(name, e1,e2) ->
    print_string "Op(";
    print_string name;
    print_string ", ";
    affiche_expr e1;
    print_string ", ";
    affiche_expr e2;
    print_string ")";
  | If(e1, e2, e3) ->
    (print_string "If(";
    affiche_expr e1;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ")";)
  | Let(x, e2, e3, recursive) ->
    (print_string "Let(";
    print_string x;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ", ";
    print_bool recursive;
    print_string ")";)
  | Fun(x, e) -> (
    print_string "Fun(";
    print_string x;
	  print_string ", ";
    affiche_expr e;
    print_string ")";)
  | Unit -> print_string "Unit"
  | Tuple(lst) -> (
    print_string "Tuple(";
    List.iter (fun x -> affiche_expr x; print_string ", ") lst;
    print_string ")";
  )

(*valeurs*)
type valeur = 
  | VU
  | VI of int
  | VB of bool
  | VF of env*string*expr
  (*Thx zoé for the idea*)
  | VF_buildin of (env -> valeur -> valeur)
  | VT of valeur list
  | Boom

  (*environments*)
and env = (string * valeur) list

let prInt _env = function
  | VI x -> print_int x;print_newline(); VI x
  | _ -> Boom;;

let empty_env = [
  ("prInt", (VF_buildin prInt))
]

let rec affiche_val v = 
  match v with 
  | VU -> print_string "Unit"
  | VI k -> print_int k
  | VB k -> print_bool k
  | VF (_env, x, e) -> 
    print_string "F(";
    print_string x;
    print_string ", ";
    affiche_expr e; 
    print_string ")";
  | VT(lst) -> (
    print_string "(";
    List.iter (fun x -> affiche_val x; print_string ", ") lst;
    print_string ")";
  )
  | VF_buildin(_) -> 
    print_string "Buildin_func";
  | Boom -> print_string "Boom"

let rec compare_val val1 val2 = match (val1, val2) with
  | (VU, VU) -> true
  | (VB k1,VB k2) -> (k1 = k2)
  | (VI k1,VI k2) -> (k1 = k2)
  | (VT lst1, VT lst2) -> compare_tuple compare_val lst1 lst2
  | _ -> false

let print_env env = List.iter (fun (x, e) ->
  print_string x; print_string " -> "; affiche_val e; print_newline ();
  ) env;;

