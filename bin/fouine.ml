open Lib

let nom_fichier = ref ""

let recupere_entree () =
  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    [] (* la liste des options, vide *)
    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    ""; (* le message d'accueil, qui est vide *)
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse () 
  with e -> (Printf.printf "Asteriiiiiiiiiiiiiiix\n"; raise e)

let run () =
  try
    let saisie = recupere_entree () in
    let _ = (Expr.affiche_expr saisie; print_newline ()) in (*to desactivate for tests*)
    let out = Eval.eval saisie [] in
    let _ = (Expr.affiche_val out; print_newline ()) in
    flush stdout
  with e -> raise e

let _ = run ()

