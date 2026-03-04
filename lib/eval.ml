open Expr
open Exceptions
open Result

let size = 1000;;
let heap = {
  array = Array.make size Boom;
  last_free = 0;
  size = size;
};;

(*gestion récursive des patterns*)
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

(* évaluation d'une expression en une valeur *)
(*On fait de façon monadique avec Result*)
let rec eval (value : expr) (env : (string * valeur) list) : 
  (valeur, valeur) result = 
  match value with 
  | Int k -> ok (VI k)
  | Bool b -> ok (VB b)
  | String s -> (match List.assoc_opt s env with 
    | Some v1 -> ok v1
    | None -> raise (UnknownVariable s))
  | If(e1, e2, e3) -> 
    let* v = eval e1 env in (
      match v with
      | VB true -> eval e2 env
      | VB false -> eval e3 env
      | _ -> raise WrongType
    )
  | App(e1, e2) -> 
    let* v1 = eval e1 env in let* v2 = eval e2 env in (
      match v1 with 
      | VF(env, pattern, e) -> (
        (*eval e (((x, v2))::env);*)
        let pat_list = pattern_match pattern v2 in (
          match pat_list with
          | None -> raise PatternUnmatched
          | Some plst -> eval e (plst@env)
        )
      )
      | VF_buildin(func) -> ok (func heap env v2);
      | _ -> raise WrongType;
    )
  | Let(pat, e1, e2, false) -> 
    let* v1 = eval e1 env in
    let pat_list = pattern_match pat v1 in 
    (
      match pat_list with 
      | None -> raise PatternUnmatched
      | Some plst -> 
        eval e2 (plst@env)
    );
  | Let(pat, e1, e2, true) -> (match pat with
    (*force pat to be a val only for now*)
    | PVar s -> (
        let* v1 = eval e1 env in (
        match v1 with
        | VF(env, pat, expr) -> (
            let rec new_env = ((s, VF(new_env, pat, expr))::env) in
            eval e2 new_env;
          )
        | _ -> (raise LetRecWronglyFormed);
      ))
    | _ -> raise UnimplementedError
  )
  | Fun(pat, e) -> ok (VF(env, pat, e));
  | Op (name, e1, e2) -> (
    (*une fonction auxiliaire pour les opérateurs de base*)
    let func = match name with
      | "+" -> opint env ( + )
      | "-" -> opint env ( - )
      | "/" -> opint env ( / )
      | "*" -> opint env ( * )
      | "<" -> opibool env ( < )
      | "<=" -> opibool env ( <= )
      | ">" -> opibool env ( > )
      | ">=" -> opibool env ( >= )
      | "@" -> opilist env ( @ )
      | "::" -> fun e1 e2 -> (
        let* v1 = eval e1 env in
        let* v2 = eval e2 env in match v2 with
        | VL l -> ok (VL (v1::l))
        | _ -> raise WrongType
      )
      (*duplication de code car les optimisations ne sont pas les mêmes pour && et ||*)
      | "&&" -> fun e1 e2 -> (
        let* v1 = eval e1 env in match v1 with
        | VB false -> ok (VB false)
        | VB true -> (let* v2 = eval e2 env in match v2 with
          | (VB k2) -> ok (VB k2)
          | _ -> raise WrongType)
        | _ -> raise WrongType
      ) 
      | "||" -> fun e1 e2 -> (
        let* v1 = eval e1 env in match v1 with
        | VB true -> ok (VB true)
        | VB false -> (let* v2 = eval e2 env in match v2 with
          | (VB k2) -> ok (VB k2)
          | _ -> raise WrongType)
        | _ -> raise WrongType
      ) 
      | "=" -> (fun x y -> 
        let* v1 = eval x env in
        let* v2 = eval y env in
        ok (VB (compare_val v1 v2)))
      | "<>" -> (fun x y -> 
        let* v1 = eval x env in
        let* v2 = eval y env in
        ok (VB (not (compare_val v1 v2))))
      | _ -> (fun _ _ -> raise UnimplementedError)
    in func e1 e2
    )
  | Tuple lst ->
    List.rev lst |> eval_list env |> Result.map (fun vs -> VT (List.rev vs))
  | Seq(e1, e2) -> let _ = eval e1 env in eval e2 env
  | LinkedList lst ->
      let* vs = eval_list env lst in
      Ok (VL vs)
  | Match(e1, lst) ->
    let* v1 = eval e1 env in 
    search lst v1 env;
  | Try (e1, lst) -> (
      match eval e1 env with
      | Ok v1 -> Ok v1
      | Error v -> search lst v env
    );
  | Raise(e) ->
    let* v = eval e env in
    match v with
    | VI n -> error (VE (VI n))
    | _ -> failwith "E a besoin d'un int"

and opint env func e1 e2 = 
  let* v2 = (eval e2 env) in let* v1 = (eval e1 env) in
    match (v1, v2) with
      | (VI x, VI y) -> ok (VI (func x y))
      | _ -> raise WrongType

and opibool env func e1 e2 = 
  let* v2 = (eval e2 env) in let* v1 = (eval e1 env) in
    match (v1, v2) with
      | (VI x, VI y) -> ok (VB (func x y))
      | _ -> raise WrongType

and eval_list env lst =
  match lst with
  | [] -> Ok []
  | x :: xs ->
      let* v  = eval x env in
      let* vs = eval_list env xs in
      Ok (v :: vs)

and opilist env func e1 e2 = 
  let* v2 = (eval e2 env) in let* v1 = (eval e1 env) in
    match (v1, v2) with
      | (VL x, VL y) -> Ok (VL (func x y))
      | _ -> raise WrongType

and search mylst v1 env = (
  match mylst with 
  | [] -> failwith "Pattern non trouve !!";
  | (p, e2)::xs -> 
    let pm = pattern_match p v1 in (
      match pm with
      | None -> search xs v1 env
      | Some plst -> eval e2 (plst@env)
    )
  )
;;
