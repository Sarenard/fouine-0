open Expr

let size = 1000;;
let heap = {
  array = Array.make size Boom;
  last_free = 0;
  size = size;
};;

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
  | Unit -> VU
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
  | Ref(e) -> 
    let v = eval e env in
    VR (set_new v heap)
  | Bang(s) -> 
    let v = eval (String s) env in (match v with
    | VR k -> heap.array.(k)
    | _ -> Boom)
  | Assign(s, e) -> 
    let v = eval (String s) env in (match v with
    | VR k -> (heap.array.(k) <- eval e env); VU
    | _ -> Boom)
  | Seq(e1, e2) -> let _ = eval e1 env in eval e2 env

and opint env func e1 e2 = 
  let v2 = (eval e2 env) in let v1 = (eval e1 env) in
    match (v1, v2) with
      | (VI x, VI y) -> (VI (func x y))
      | _ -> Boom;;