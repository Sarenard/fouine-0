{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let bool = "true"|"false"
               
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
   	     	   	               (*   en faisant cet appel récursif à "token" *)
  | '\n'            { EOL }   (*EndOfLine ; à noter que la fin de fichier se note "eof" *)
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "if"             { IF }
  | "||"             { OR }
  | "&&"             { AND }
  | "then"             { THEN }
  | "else"             { ELSE }
  | bool as b { BOOL (bool_of_string b)}
  | nombre as s { INT (int_of_string s) }
