%{
open Expr
%}

/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token BANG ASSIGN SEQ SEQQ
%token OR AND
%token PLUS TIMES MINUS DIV
%token BEGIN END
%token LPAREN RPAREN COMMA
%token MATCH WITH PIPE
%token LET IF THEN ELSE IN FUN ARROW REC
%token L LE G GE NE EQ
%token EOF
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR

/* PARTIE 3, on donne les associativités et on classe les priorités *********** */
/* priorité plus grande sur une ligne située plus bas */
%nonassoc ELSE IN ARROW  
%nonassoc below_PIPE
%nonassoc LE GE NE
%nonassoc L G
%left PIPE
%left EQ
%right AND
%right OR
%left PLUS MINUS
%left TIMES DIV
%right SEQ
%right ASSIGN

/* PARTIE 4, le point d'entrée ******************************************* */
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */

/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:
  | e=toplevel EOF { e }
  | e=toplevel SEQQ EOF { e }

toplevel:
  | e1 = toplevel SEQQ e2 = expression {Seq(e1, e2)}
  | LET e1=pattern EQ e2=expression SEQQ e3 = expression { Let(e1,e2,e3, false) }
  | LET REC e1=pattern EQ e2=expression SEQQ e3=expression { Let(e1,e2,e3, true) }
  | e = expression {e}

expression:			   
  | e1 = expression SEQ e2 = expression { Seq(e1, e2) } 
  | controwlflow {$1}

controwlflow:
  | IF e1=expression THEN e2=expression ELSE e3=expression { If(e1,e2,e3) }
  | LET e1=pattern EQ e2=expression IN e3=expression { Let(e1,e2,e3, false) }
  | LET REC e1=pattern EQ e2=expression IN e3=expression { Let(e1,e2,e3, true) }
  | FUN args=pattern+ ARROW e=expression { List.fold_right (fun x acc -> Fun(x,acc)) args e}
  | s=VAR ASSIGN e=expression                { Assign(s, e) }
  | MATCH e=expression WITH m=match_inner {Match(e, m)}
  | operator {$1}

match_inner:
  | first = match_first rest = match_rest {first :: rest}

match_first:
  | PIPE p=pattern ARROW e=expression {(p,e)}
  | p=pattern ARROW e=expression {(p,e)}

match_rest:
  | %prec below_PIPE {[]} (*weird precedence*)
  | PIPE p=pattern ARROW e=expression lst=match_rest {(p,e)::lst}

operator:
  | e1=operator OR e2=operator     { Op("||", e1, e2) }
  | e1=operator AND e2=operator     { Op("&&", e1, e2) }
  | e1=operator EQ e2=operator     { Op("=", e1, e2) }
  | e1=operator DIV e2=operator      { Op("/", e1, e2) }
  | e1=operator PLUS e2=operator      { Op("+", e1, e2) }
  | e1=operator MINUS e2=operator     { Op("-", e1, e2) }
  | e1 = operator TIMES e2 = operator { Op("*", e1, e2) }
  | e1 = operator LE e2 = operator { Op("<=", e1, e2) }
  | e1 = operator GE e2 = operator { Op(">=", e1, e2) }
  | e1 = operator NE e2 = operator { Op("<>", e1, e2) }
  | e1 = operator L e2 = operator { Op("<", e1, e2) }
  | e1 = operator G e2 = operator { Op(">", e1, e2) }
  | MINUS e=operator { Op("-", Int 0, e)}
  | e = applic { e }

applic:
  | e1=applic e2=expr_ident {App(e1, e2)}
  | expr_ident {$1}

expr_ident:
  | i=INT {Int i}
  | b=BOOL {Bool b}
  | BANG s=VAR { App(String "!", String s) }
  | s=VAR {String s}
  | LPAREN RPAREN                    { Tuple [] }
  | LPAREN e=expression RPAREN {e}
  (*TODO : a tuple is a tuple even without parentheses*)
  | LPAREN xs=expr_list COMMA x=expression RPAREN            { Tuple (xs @ [x]) } 
  | BEGIN END                             { Tuple [] }
  | BEGIN e=expression END                { e }

expr_list:
  | x=expression                        { [x] }
  | xs=expr_list COMMA x=expression     { xs @ [x] }

pattern:
  | i=INT {PInt i}
  | b=BOOL {PBool b}
  | LPAREN x=pattern RPAREN  { x } 
  | LPAREN RPAREN  { PTuple [] }
  | LPAREN xs=pattern_list COMMA x=pattern RPAREN  { PTuple (xs @ [x]) } 
  | s=VAR {PVar s}

pattern_list:
  | x = pattern { [x] }
  | xs = pattern_list COMMA x = pattern {xs @ [x] }