open Expressions

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
  | Add(e1,e2) -> aff_aux "Add(" e1 e2
  | Mul(e1,e2) -> aff_aux "Mul(" e1 e2
  | Min(e1,e2) -> aff_aux "Min(" e1 e2
  | Or(e1,e2) -> (
    affiche_expr e1;
	  print_string " || ";
    affiche_expr e2;
  )
  | And(e1,e2) -> (
    affiche_expr e1;
	  print_string " && ";
    affiche_expr e2;
  )
  | If(e1, e2, e3) ->
    (print_string "If(";
    affiche_expr e1;
	  print_string ", ";
    affiche_expr e2;
	  print_string ", ";
    affiche_expr e3;
    print_string ")";)

let affiche_val v = 
  match v with 
  | VI k -> print_int k
  | VB k -> print_bool k
  | Boom -> print_string "Boom"
                      
