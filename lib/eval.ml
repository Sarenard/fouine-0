open Expr

let size = 1000;;
let heap = {
  array = Array.make size Boom;
  last_free = 0;
  size = size;
};;

let rec pattern_match (pat: pattern) (value: valeur) : ((string*valeur) list) option
 = match (pat, value) with
  | (PInt k1, VI k2) when k1 = k2 -> Some []
  | (PBool b1, VB b2) when b1 = b2 -> Some []
  | (PVar x, value) -> if x = "_" then Some [] else Some [(x,value)]
  | (PTuple patlist, VT valuelist) ->
    pattern_match_list [] patlist valuelist
  | _ -> None
and pattern_match_list (stack : (string*valeur) list) (pat : pattern list) (vals : valeur list)
  : (string*valeur) list option = match (pat, vals) with
  | [], [] -> Some stack
  | [], _ | _, [] -> None
  | p::ps, v::vs -> (
    match pattern_match p v with
    | Some lst -> pattern_match_list (lst@stack) ps vs
    | None -> None
  );
;;

(* évaluation d'une expression en une valeur *)
let rec eval value env = match value with 
  | Int k -> VI k
  | Bool b -> VB b
  | String s -> (match List.assoc_opt s env with 
    | Some v1 -> v1
    | None -> Boom)
  | If(e1, e2, e3) -> 
    let v = eval e1 env in (
      match v with
      | (VB true) -> eval e2 env
      | (VB false) -> eval e3 env
      | _ -> Boom
    )
  | App(e1, e2) -> 
    let v1, v2 = eval e1 env, eval e2 env in (
      match v1 with 
      | VF(env, pattern, e) -> (
        (*eval e (((x, v2))::env);*)
        let pat_list = pattern_match pattern v2 in (
          match pat_list with
          | None -> Boom
          | Some plst -> eval e (plst@env)
        )
      )
      | VF_buildin(func) -> (func heap env v2);
      | _ -> Boom;
    )
  (*TODO : handle _*)
  | Let(pat, e1, e2, false) -> 
    let v1 = eval e1 env in
    let pat_list = pattern_match pat v1 in 
    (
      match pat_list with 
      | None -> Boom
      | Some plst -> 
        eval e2 (plst@env)
    );
  | Let(pat, e1, e2, true) -> (match pat with
    (*force pat to be a val only for now*)
    | PVar s -> (
        let v1 = eval e1 env in (
        match v1 with
        | VF(env, pat, expr) -> (
            let rec new_env = ((s, VF(new_env, pat, expr))::env) in
            eval e2 new_env;
          )
        | _v1 -> Boom;
      ))
    | _ -> Boom (*not implemented yet*)
  )
  | Fun(pat, e) -> VF(env, pat, e)
  | Op (name, e1, e2) -> (
    let func = match name with
      | "+" -> opint env ( + )
      | "-" -> opint env ( - )
      | "*" -> opint env ( * )
      | "&&" -> fun e1 e2 -> (
        let v1 = eval e1 env in match v1 with
        | VB false -> VB false
        | VB true -> (let v2 = eval e2 env in match v2 with
          | (VB k2) -> VB k2
          | _ -> Boom)
        | _ -> Boom
      ) 
      | "||" -> fun e1 e2 -> (
        let v1 = eval e1 env in match v1 with
        | VB true -> VB true
        | VB false -> (let v2 = eval e2 env in match v2 with
          | (VB k2) -> VB k2
          | _ -> Boom)
        | _ -> Boom
      ) 
      (*wow thats strange, maybe i will want to change that ?*)
      | "=" -> (fun x y -> VB (compare_val (eval x env) (eval y env)))
      | _ -> (fun _ _ -> Boom)
    in func e1 e2
    )
  | Tuple(lst) -> VT (List.rev (List.map (fun x -> eval x env) (List.rev lst)))
  | Bang(s) -> 
    let v = eval (String s) env in (match v with
    | VR k -> heap.array.(k)
    | _ -> Boom)
  | Assign(s, e) -> 
    let v = eval (String s) env in (match v with
    | VR k -> (heap.array.(k) <- eval e env); VU
    | _ -> Boom)
  | Seq(e1, e2) -> let _ = eval e1 env in eval e2 env
  | Match(e1, lst) ->
    let v1 = eval e1 env in 
    let rec search mylst = (
      match mylst with 
      | [] -> failwith "Pattern non trouvé !!";
      | (p, e2)::xs -> 
        let pm = pattern_match p v1 in (
          match pm with
          | None -> search xs
          | Some plst -> eval e2 (plst@env)
        )
    )
    in search lst;


and opint env func e1 e2 = 
  let v2 = (eval e2 env) in let v1 = (eval e1 env) in
    match (v1, v2) with
      | (VI x, VI y) -> (VI (func x y))
      | _ -> Boom;;