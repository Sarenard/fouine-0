{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let var = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let bool = "true"|"false"
               
rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\n' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
   	     	   	               (*   en faisant cet appel récursif à "token" *)
  | ";;"             { SEQQ }
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '('             { LPAREN }
  | ','             { COMMA }
  | ')'             { RPAREN }
  | "if"             { IF }
  | "||"             { OR }
  | "fun"             { FUN }
  | "->"             { ARROW }
  | "let"             { LET }
  | "="             { EQ }
  | "in"             { IN }
  | "&&"             { AND }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "rec"             { REC }
  | "begin"           { BEGIN }
  | "end"             { END }
  | ";"             { SEQ }
  | "!"             { BANG }
  | ":="             { ASSIGN }
  | bool as b { BOOL (bool_of_string b)}
  | nombre as s { INT (int_of_string s) }
  | var as s { VAR s }
