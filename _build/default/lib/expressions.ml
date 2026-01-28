(* un type pour des expressions simples *)
type expr =
    Int of int
  | Bool of bool
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr
  | Or of expr*expr
  | And of expr*expr
  | If of expr*expr*expr
  | PrInt of expr

let print_bool = function
  | true  -> print_string "true"
  | false -> print_string "false"

let prInt x = print_int x;print_newline(); x;;

(* sémantique opérationnelle à grands pas *)
type valeur = 
    VI of int
  | VB of bool
  | Boom

let affiche_valeur v =
  match v with 
  | VI k -> (print_int k; print_newline ())
  | VB k -> (print_bool k; print_newline ())
  | Boom -> (print_string "Boom"; print_newline ())

(* évaluation d'une expression en une valeur *)
let rec eval = function
  | Int k -> VI k
  | Bool k -> VB k
  | Add(e1,e2) ->
    let v1, v2 = eval e1, eval e2 in (
      match (v1,v2) with
      | (VI k1,VI k2) -> VI (k1+k2)
      | _ -> Boom
    )
  | Mul(e1,e2) -> 
    let v1, v2 = eval e1, eval e2 in (
      match (v1,v2) with
      | (VI k1,VI k2) -> VI (k1 * k2)
      | _ -> Boom
    )
  | Min(e1,e2) -> 
    let v1, v2 = eval e1, eval e2 in (
      match (v1,v2) with
      | (VI k1,VI k2) -> VI (k1 - k2)
      | _ -> Boom
    )
  | If(e1, e2, e3) -> 
    let v = eval e1 in (
      match v with
      | (VB true) -> eval e2
      | (VB false) -> eval e3
      | _ -> Boom
    )
  | Or(e1,e2) -> 
    let v1, v2 = eval e1, eval e2 in (
      match (v1,v2) with
      | (VB k1,VB k2) -> VB (k1 || k2)
      | _ -> Boom
    )
  | And(e1,e2) -> 
    let v1, v2 = eval e1, eval e2 in (
      match (v1,v2) with
      | (VB k1,VB k2) -> VB (k1 && k2)
      | _ -> Boom
    )
  | PrInt(e) -> 
    let v = eval e in (
      match v with 
      | (VI k) -> (VI (prInt k));
      | _ -> Boom
    )