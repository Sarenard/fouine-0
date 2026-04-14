open Expr
open Exceptions
open Result

let size = 1000000;;
let heap = {
  array = Array.make size Boom;
  last_free = 0;
  size = size;
};;

(* Etant donnés un pattern et une valeur, on produit None si le pattern ne correspond pas,
sinon une liste de bindings *)
let rec pattern_match (pat: pattern) (value: valeur) : ((string*valeur) list) option = 
  match (pat, value) with
  | (PInt k1, VI k2) when k1 = k2 -> Some []
  | (PBool b1, VB b2) when b1 = b2 -> Some []
  | (PVar x, value) -> if x = "_" then Some [] else Some [(x,value)]
  | (PTuple patlist, VT valuelist) ->
    pattern_match_list [] patlist valuelist
    | (PNil, VL []) -> Some []
    | (PCons(x,xs), VL(y::ys)) -> (match (pattern_match xs (VL ys)) with
      | Some l -> (match (pattern_match x y) with
        | Some l' -> Some (l @ l')
        | None -> None
      )
    | None -> None
    )
  | (PE pat, VE valeur) -> pattern_match pat valeur
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

let ( let* ) = Result.bind;;

(* évaluation d'une expression en une valeur, en style par continuations *)
let rec eval_cps (value : expr) (env : (string * valeur) list) k kE : 
  (valeur, valeur) result = 
  match value with 
  | Int n -> k (VI n)
  | Bool b -> k (VB b)
  | String s -> (match List.assoc_opt s env with 
    | Some v1 -> k v1
    | None -> raise (UnknownVariable s))
  | If(e1, e2, e3) -> 
    eval_cps e1 env (
      fun v -> match v with
      | VB true -> eval_cps e2 env k kE
      | VB false -> eval_cps e3 env k kE
      | _ -> raise WrongType
      ) kE
  | App(e1, e2) -> 
    eval_cps e2 env (fun v2 -> eval_cps e1 env (fun v1 ->
      match v1 with 
      | VF(env, pattern, e) -> (
        (*eval e (((x, v2))::env);*)
        let pat_list = pattern_match pattern v2 in (
          match pat_list with
          | None -> raise PatternUnmatched
          | Some plst -> eval_cps e (plst@env) k kE
        )
      )
      | VF_buildin(func) -> k (func heap env v2);
      | _ -> raise WrongType;
    ) kE) kE
  | Let(pat, e1, e2, false) -> 
    eval_cps e1 env (fun v1 ->
    (
      match pattern_match pat v1 with 
      | None -> raise PatternUnmatched
      | Some plst -> 
        eval_cps e2 (plst@env) k kE
    )) kE
  | Let(pat, e1, e2, true) -> eval_cps e1 env (
    fun v1 -> match pat with
    (*force pat to be a val only for now*)
    | PVar s -> (
        match v1 with
        | VF(env, pat, expr) -> (
            let rec new_env = ((s, VF(new_env, pat, expr))::env) in
            eval_cps e2 new_env k kE;
          )
        | _ -> (raise LetRecWronglyFormed);
      )
    | _ -> raise UnimplementedError
  ) kE
  | Fun(pat, e) -> k (VF(env, pat, e));
  | Op (name, e1, e2) -> (
    (*une fonction auxiliaire pour les opérateurs de base*)
    (match name with
      | "+" -> opint env ( + )
      | "-" -> opint env ( - )
      | "/" -> opint env ( / )
      | "*" -> opint env ( * )
      | "<" -> opibool env ( < )
      | "<=" -> opibool env ( <= )
      | ">" -> opibool env ( > )
      | ">=" -> opibool env ( >= )
      | "@" -> opilist env ( @ )
      | "::" -> fun e1 e2 k kE -> (
        eval_cps e1 env (
          fun v1 -> eval_cps e2 env (
            fun v2 -> match v2 with
            | VL l -> k (VL (v1::l))
            | _ -> raise WrongType
          ) kE
        ) kE
      )
      | "&&" -> opbbool env ( && ) false
      | "||" -> opbbool env ( || ) true
      | "=" -> (fun x y k kE -> 
        eval_cps x env (
          fun v1 -> eval_cps y env (
            fun v2 -> k (VB (compare_val v1 v2))
          ) kE
        ) kE)
      | "<>" -> (fun x y k kE -> 
        eval_cps x env (
          fun v1 -> eval_cps y env (
            fun v2 -> k (VB (not (compare_val v1 v2)))
          ) kE
        ) kE)
      | _ -> (fun _ _ -> raise UnimplementedError)
    ) e1 e2 k kE
    )
  | Tuple lst ->
    eval_list env (List.rev lst) (
      fun lst -> k (VT (List.rev lst))
    ) kE
  | Seq(e1, e2) -> eval_cps e1 env (fun _ -> eval_cps e2 env k kE) kE
  | LinkedList lst ->
      eval_list env (List.rev lst) (fun vs -> k (VL (List.rev vs))) kE
  | Match(e1, lst) ->
    eval_cps e1 env (fun v1 -> search lst v1 env k kE) kE 
  | Try (e1, lst) -> 
    eval_cps e1 env (fun v1 -> k v1) (fun v -> search lst v env k kE)
  | Raise(e) ->
    eval_cps e env (
      fun v -> match v with
      | VE x -> kE (VE x)
      | _ -> failwith "cannot raise non exception"
      ) kE
(* wrapper pour gerer les operateurs entiers de fouine avec les operateurs natifs de caml *)
and opint env func e1 e2 k kE = 
  eval_cps e2 env (fun v2 -> eval_cps e1 env (fun v1 ->
    match (v1, v2) with
      | (VI x, VI y) -> k (VI (func x y))
      | _ -> raise WrongType) kE
    ) kE

and opibool env func e1 e2 k kE = 
  eval_cps e2 env (fun v2 -> eval_cps e1 env (fun v1 ->
    match (v1, v2) with
      | (VI x, VI y) -> k (VB (func x y))
      | _ -> raise WrongType) kE
    ) kE

and eval_list env lst k kE =
  match lst with
  | [] -> k []
  | x :: xs ->
      eval_cps x env (
        fun v -> eval_list env xs (
          fun vs -> k (v :: vs)
        ) kE
      ) kE

and opilist env func e1 e2 k kE = 
  eval_cps e2 env (
    fun v2 -> eval_cps e1 env (
      fun v1 -> 
        match (v1, v2) with
        | (VL x, VL y) -> k (VL (func x y))
        | _ -> raise WrongType
    ) kE
  ) kE
and opbbool env func skipper e1 e2 k kE = (* skipper est la valeur absorbante sur laquelle on optimise *)
  eval_cps e1 env (
    fun v1 -> match v1 with
    | VB b when b = skipper -> k (VB skipper)
    | VB _ -> (eval_cps e2 env (
      fun v2 -> match v2 with
      | (VB k2) -> k (VB k2)
      | _ -> raise WrongType)
      ) kE
    | _ -> raise WrongType
  ) kE
(* recherche le bon pattern dans un match with *)
and search mylst v1 env k kE = (
  match mylst with 
  | [] -> failwith "Pattern non trouve !!";
  | (p, e2)::xs -> 
    let pm = pattern_match p v1 in (
      match pm with
      | None -> search xs v1 env k kE
      | Some plst -> eval_cps e2 (plst@env) k kE
    )
  )
;;


let eval (value : expr) (env : (string * valeur) list) : (valeur, valeur) result = 
  eval_cps value env (fun x -> ok x) (fun e -> error e)