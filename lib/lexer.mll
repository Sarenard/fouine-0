{
  (* prélude du fichier *)
  open Parser
          
}

(* définitions d'expressions régulières *)
let chiffre = ['0'-'9']
let nombre = chiffre+
let var = ['a'-'z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let bool = "true"|"false"
               
rule token = parse
  | [' ' '\n' '\t']     { token lexbuf }

  | ";;"             { SEQQ }
  | ";"             { SEQ }

  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '/'             { DIV }
  | "="             { EQ }
  | "||"             { OR }
  | "&&"             { AND }
  | "<"             { L }
  | "<="             { LE }
  | ">"             { G }
  | ">="             { GE }
  | "<>"             { NE }

  | "::"            { LST_PREPEND }
  | "@"             { LST_CONCAT }
  | "["             { LBRACKET }
  | "]"             { RBRACKET }

  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "begin"           { BEGIN }
  | "end"             { END }

  | "try" { TRY }
  | "E" { E }
  | "raise" { RAISE }

  | "if"             { IF }
  | "then"             { THEN }
  | "else"             { ELSE }

  
  | "fun"             { FUN }
  | "function"        { FUNCTION }
  | "->"             { ARROW }
  
  | "let"             { LET }
  | "rec"             { REC }
  | "in"             { IN }
  
  | ','             { COMMA }
  
  | "!"             { BANG }
  | ":="             { ASSIGN }
  
  | "match"             { MATCH }
  | "with"             { WITH }
  | "|"             { PIPE }
  
  | bool as b { BOOL (bool_of_string b)}
  | nombre as s { INT (int_of_string s) }
  | var as s { VAR s }
  
  | eof               { EOF }