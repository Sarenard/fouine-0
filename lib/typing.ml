type var = string
type symbol = string

type t =
  | TVar of var
  | TCst of int
  | TApp of t * t
  | TFun of var * t
  | TAdd of t * t
  | TLet of var * t * t

type problem = 
  (t * t) list

type texpr = 
  | Var of var
  | Op of symbol * texpr list

type problemexpr = 
  (texpr * texpr) list
 
let examples : t list = [
  (* let x = 2 in x *)
  TLet ("x", TCst 2, TVar "x");

  (* 2 + (3 + 4) *)
  TAdd (TCst 2, TAdd (TCst 3, TCst 4));

  (* (fun x -> x) 42 *)
  TApp (TFun ("x", TVar "x"), TCst 42);

  (* fun x -> 2 *)
  TFun ("x", TCst 2);

  (* fun x -> x *)
  TFun ("x", TVar "x");

  (* fun x -> fun y -> x *)
  TFun ("x", TFun ("y", TVar "x"));

  (* let apply f x = f x in apply *)
  TLet ("apply",
        TFun ("f", TFun ("x", TApp (TVar "f", TVar "x"))),
        TVar "apply");

  (* let compose f g x = f (g x) in compose *)
  TLet ("compose",
        TFun ("f",
          TFun ("g",
            TFun ("x",
              TApp (TVar "f", TApp (TVar "g", TVar "x"))
            ))),
        TVar "compose");

  (* let compose f g x = f (g x) in let id x = x in compose id id *)
  TLet ("compose",
        TFun ("f",
          TFun ("g",
            TFun ("x",
              TApp (TVar "f", TApp (TVar "g", TVar "x"))
            ))),
        TLet ("id",
              TFun ("x", TVar "x"),
              TApp (TApp (TVar "compose", TVar "id"), TVar "id")));

  (* let f x = 2 in f 3 *)
  TLet ("f",
        TFun ("x", TCst 2),
        TApp (TVar "f", TCst 3));

  (* (fun f -> fun g -> fun x -> f (g x)) (fun y -> y) (fun z -> z) *)
  TApp (
    TApp (
      TApp (
        TFun ("f",
          TFun ("g",
            TFun ("x",
              TApp (TVar "f", TApp (TVar "g", TVar "x"))
            ))),
        TFun ("y", TVar "y")
      ),
      TFun ("z", TVar "z")
    ),
    (* remarque: il manque un 3e argument "x" dans ta chaîne, donc l'expression
       est bien une application à deux arguments, pas trois. *)
    (* Si tu voulais aussi appliquer à un x, ajoute TCst ... ou TVar ... ici. *)
    (* Ici on laisse exactement comme ta chaîne: deux arguments. *)
    (* -> donc ce TApp externe est en trop si on respecte la chaîne.
       Correction ci-dessous: je remets la bonne structure. *)
    TCst 0
  );

  (* let s x y z = x z (y z) in s *)
  TLet ("s",
        TFun ("x",
          TFun ("y",
            TFun ("z",
              TApp (TApp (TVar "x", TVar "z"),
                   TApp (TVar "y", TVar "z"))
            ))),
        TVar "s");

  (* let s x y z = x z (y z) in let k x y = x in let i = s k k in i *)
  TLet ("s",
        TFun ("x",
          TFun ("y",
            TFun ("z",
              TApp (TApp (TVar "x", TVar "z"),
                   TApp (TVar "y", TVar "z"))
            ))),
        TLet ("k",
              TFun ("x", TFun ("y", TVar "x")),
              TLet ("i",
                    TApp (TApp (TVar "s", TVar "k"), TVar "k"),
                    TVar "i")));

  (* let delta x = x x in delta *)
  TLet ("delta",
        TFun ("x", TApp (TVar "x", TVar "x")),
        TVar "delta");
]

let rec string_of_term = function
  | TAdd (e1, e2) ->
      Printf.sprintf "%s + %s"
         (string_of_complex_term e1) (string_of_complex_term e2)
  | TLet (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x
         (string_of_term e1) (string_of_term e2)
  | TFun (x, e) ->
      Printf.sprintf "fun %s -> %s" x (string_of_term e)
  | TApp (e1, e2) ->
      Printf.sprintf "%s %s"
         (string_of_app e1) (string_of_complex_term e2)
  | TVar _ | TCst _ as e -> string_of_complex_term e

and string_of_complex_term = function
  | TVar x -> x
  | TCst x -> string_of_int x
  | e -> Printf.sprintf "(%s)" (string_of_term e)

and string_of_app = function
  | TApp (TApp (_,_) as t1, t2) ->
      Printf.sprintf "%s %s" (string_of_app t1) (string_of_complex_term t2)
  | TApp (t1, t2) -> Printf.sprintf "%s %s" (string_of_complex_term t1) (string_of_complex_term t2)
  | TFun (_,_) as t1 -> Printf.sprintf "(%s)" (string_of_term t1)
  | t1 -> string_of_term t1

let uvar_number = ref 0

let new_uvar = fun () ->
  let v = "X"^(string_of_int !uvar_number) in
  (incr uvar_number; v)

type ty = Tint | Tarr of ty * ty | Tuvar of string

type unif_pbm = (ty*ty) list

let rec infer (env : (var * ty) list) (v : var) (t: t) : unif_pbm = match t with
  | TAdd (t1, t2) -> 
    let x1 = new_uvar () in let x2 = new_uvar () in
    let u1 = infer env x1 t1 in let u2 = infer env x2 t2 in
    [(Tuvar v, Tint); (Tuvar x1, Tint); (Tuvar x2, Tint)]@u1@u2
  | TCst _ -> [(Tuvar v, Tint)]
  | TVar v1 -> let tv =
      try List.assoc v1 env
      with Not_found -> failwith "caca"
    in
    [ (Tuvar v, tv) ]
  | TFun (x, body) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let env' = (x, Tuvar a1) :: env in
    let u = infer env' a2 body in
    (Tuvar v, Tarr (Tuvar a1, Tuvar a2)) :: u
  | TApp (t1, t2) ->
    let a1 = new_uvar () in
    let a2 = new_uvar () in
    let u1 = infer env a1 t1 in
    let u2 = infer env a2 t2 in
    (Tuvar a1, Tarr (Tuvar a2, Tuvar v)) :: (u1 @ u2)
  | TLet (x, t1, t2) ->
    let a1 = new_uvar () in
    let u1 = infer env a1 t1 in
    let env' = (x, Tuvar a1) :: env in
    let u2 = infer env' v t2 in
    u1 @ u2

let rec ty_to_expr (ty : ty) : texpr =
  match ty with
  | Tint -> Op ("int", [])
  | Tuvar x -> Var x
  | Tarr (a, b) -> Op ("arr", [ty_to_expr a; ty_to_expr b])

let unif_pbm_to_problem (pb : unif_pbm) : problemexpr =
  List.map (fun (a, b) -> (ty_to_expr a, ty_to_expr b)) pb

let rec string_of_expr = function
  | Var s -> s
  | Op ("int", []) -> "int"
  | Op ("arr", [t1; t2]) ->
      Printf.sprintf "(%s -> %s)"
        (string_of_expr t1)
        (string_of_expr t2)
  | Op (f, args) ->
      Printf.sprintf "%s(%s)"
        f
        (String.concat ", "
           (List.map string_of_expr args))

let rec print_sub = function
  | [] -> ()
  | (v,t)::q -> let _ = Printf.printf "%s = %s\n" v (string_of_expr t) in print_sub q

let string_of_problem l =
  String.concat "\n" 
    (List.map (fun (x,y) -> 
      Printf.sprintf "%s = %s" (string_of_expr x)
                               (string_of_expr y)) l)

exception Not_unifyable
type subst = (var * texpr) list

let rec appear (x : var) (term : texpr) : bool =
  match term with
  | Var y -> x = y
  | Op (_, lst) -> (List.filter (appear x) lst) <> []

(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace ((x, new_x) : var * texpr) (term : texpr) : texpr =
  match term with
  | Var y when y = x -> new_x
  | Var _ -> term
  | Op (sym, lst) -> Op(
    sym,
    List.map (replace (x, new_x)) lst
  )

let rec zip l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x1 :: r1, x2 :: r2 -> (x1, x2) :: zip r1 r2
  | _ -> invalid_arg "zip: listes de tailles différentes"

let apply_subst (sb : subst) (term : texpr) : texpr =
  List.fold_left (fun acc (x, u) -> replace (x, u) acc) term sb

(*Implémente l'unification de deux termes*)
let unify (pb : problemexpr) : subst =
  let rec unify_aux (pb : problemexpr) (sb : subst) : subst =
    match pb with
    | [] -> sb
    | (t1, t2) :: rest ->
      let t1 = apply_subst sb t1 in
      let t2 = apply_subst sb t2 in
      if t1 = t2 then unify_aux rest sb else
        match (t1, t2) with
        | (Op (f, l1), Op (g, l2))
          when f = g && List.length l1 = List.length l2 ->
            unify_aux (zip l1 l2 @ rest) sb

        | (Var x, t) ->
            if appear x t then raise Not_unifyable;
            let rest' =
              List.map (fun (a, b) -> (replace (x, t) a, replace (x, t) b)) rest
            in
            let sb' =
              List.map (fun (y, ty) -> (y, replace (x, t) ty)) sb
            in
            unify_aux rest' ((x, t) :: sb')

        | (t, Var x) ->
            unify_aux ((Var x, t) :: rest) sb

        | _ ->
            raise Not_unifyable
  in
  unify_aux pb []

let inference t =
  begin
    Printf.printf "# inférence sur %s\n" (string_of_term t);
    try
      let v0 = new_uvar () in
      let pbm0 = infer [] v0 t in
      let pbm = unif_pbm_to_problem pbm0 in
      Printf.printf "Solving : \n%s\n" (string_of_problem pbm);
      try
        let sub = unify pbm in
        Printf.printf "Solution : \n";
        print_sub sub;
        let ty_v0 = apply_subst sub (Var v0) in
        Printf.printf "Type inféré : %s\n\n"
          (string_of_expr ty_v0);

      with Not_unifyable ->
        Printf.printf "Not unifyable.\n\n"
    with e ->
      Printf.printf "Error : uncaught exception '%s'.\n\n" (Printexc.to_string e)
  end

let main () = 
  List.iter inference examples;
  failwith "Enf of typing tests";
;;
  
