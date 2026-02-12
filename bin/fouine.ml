open Lib

let nom_fichier = ref ""
let debug = ref false

let recupere_entree () =
  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    [
      ("-d", Arg.Set debug, "Mode debug");
    ] (* la liste des options, vide *)
    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    ""; (* le message d'accueil, qui est vide *)
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse ()
  with e -> (
    Printf.printf "Asteriiiiiiiiiiiiiiix\n"; raise e
  )

let run () =
  try
    let saisie = recupere_entree () in
    let _typed = Typing.main saisie (!debug) in
    let _ = if !debug then (Expr.affiche_expr saisie; print_newline ()) else () in
    let out = Eval.eval saisie Expr.empty_env in
    let _ = if !debug then (Expr.affiche_val out; print_newline ()) else () in
    flush stdout
  with e -> raise e

let _ = run ()
