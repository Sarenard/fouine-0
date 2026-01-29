open Util

(*expressions*)
type expr =
    Int of int
  | Bool of bool
  | String of string
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | Or of expr*expr
  | And of expr*expr
  | Eq of expr*expr
  | If of expr*expr*expr
  | Let of string*expr*expr
  | Fun of string*expr
  | App of expr*expr

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
  | Eq(e1,e2) -> aff_aux "Eq(" e1 e2
  | Or(e1,e2) -> aff_aux "Or(" e1 e2
  | And(e1,e2) -> aff_aux "And(" e1 e2
  | App(e1,e2) -> aff_aux "App(" e1 e2
  | If(e1, e2, e3) ->
    (print_string "If(";
    affiche_expr e1;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ")";)
  | Let(x, e2, e3) ->
    (print_string "Let(";
    print_string x;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ")";)
  | Fun(x, e) -> (
    print_string "Fun(";
    print_string x;
	  print_string ", ";
    affiche_expr e;
    print_string ")";)

(*valeurs*)
type valeur = 
  | VI of int
  | VB of bool
  | VF of env*string*expr
  (*Thx zoé for the idea*)
  | VF_buildin of (env -> valeur -> valeur)
  | Boom

  (*environments*)
and env = (string * valeur) list

let prInt _env = function
  | VI x -> print_int x;print_newline(); VI x
  | _ -> Boom;;
let empty_env = [
  ("prInt", (VF_buildin prInt))
]

let affiche_val v = 
  match v with 
  | VI k -> print_int k
  | VB k -> print_bool k
  | VF (_env, x, e) -> 
    print_string "F(";
    print_string x;
    print_string ", ";
    affiche_expr e; 
    print_string ")";
  | VF_buildin(_) -> 
    print_string "Buildin_func";
  | Boom -> print_string "Boom"

let print_env env = List.iter (fun (x, e) ->
  print_string x; print_string " -> "; affiche_val e; print_newline ();
  ) env;;
