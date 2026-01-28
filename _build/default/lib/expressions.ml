(* un type pour des expressions arithmétiques simples *)
type expr =
    Int of int
  | Bool of bool
  | Add of expr*expr
  | Mul of expr*expr
  | Min of expr*expr

(* sémantique opérationnelle à grands pas *)

let print_bool = function
  | true  -> print_string "true"
  | false -> print_string "false"

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

