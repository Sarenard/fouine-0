open Expr

(* évaluation d'une expression en une valeur *)
let rec eval value env = match value with 
  | Int k -> VI k
  | Bool b -> VB b
  | String s -> (match List.assoc_opt s env with 
    | Some v1 -> v1
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
    let v1 = eval e1 env in (
      match v1 with
      | VB true -> VB true
      | _ -> (
        let v2 = eval e2 env in 
        match (v1,v2) with
        | (VB k1,VB k2) -> VB (k1 || k2)
        | _ -> Boom
      )
    ) 
  | And(e1,e2) ->
    let v1 = eval e1 env in (
      match v1 with
      | VB false -> VB false
      | _ -> (
        let v2 = eval e2 env in 
        match (v1,v2) with
        | (VB k1,VB k2) -> VB (k1 && k2)
        | _ -> Boom
      )
    ) 
  | App(e1, e2) ->
    let v1, v2 = eval e1 env, eval e2 env in (
      match v1 with 
      | VF(env, x, e) -> eval e (((x, v2))::env);
      | VF_buildin(func) -> (func env v2);
      | _ -> Boom
    )
  (*TODO : handle _*)
  | Let(str, e1, e2, false) -> 
    eval e2 ((str, eval e1 env)::env);
  | Let(str, e1, e2, true) -> 
    let v1 = eval e1 env in (
      match v1 with
      | VF(env, name, expr) -> (
          let rec new_env = ((str, VF(new_env, name, expr))::env) in
          eval e2 new_env;
        )
      | _v1 -> Boom;
    )
    
  | Fun(str, e) -> VF(env, str, e)
  | Eq(e1, e2) -> 
    let v1, v2 = eval e1 env, eval e2 env in (
      VB (compare_val v1 v2)
    )
  | Unit -> VU
  | Tuple(lst) -> VT (List.rev (List.map (fun x -> eval x env) (List.rev lst)))