%{
open Expr
%}

/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token EQ
%token BANG ASSIGN SEQ SEQQ
%token OR AND
%token PLUS TIMES MINUS
%token BEGIN END
%token LPAREN RPAREN COMMA
%token LET IF THEN ELSE IN FUN ARROW REC
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR

/* PARTIE 3, on donne les associativités et on classe les priorités *********** */
/* priorité plus grande sur une ligne située plus bas */
%nonassoc ELSE IN ARROW
%left EQ
%right AND
%right OR
%left PLUS MINUS
%left TIMES
%right SEQ
%right ASSIGN

/* PARTIE 4, le point d'entrée ******************************************* */
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */

/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
e=expression SEQQ { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */


/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */                                                                                
expression:			   
   /* on appelle i l'attribut associé à INT */
  | i=INT                             { Int i }
  | b=BOOL                             { Bool b }
  | s=VAR                             { String s }
  | e1=expression PLUS e2=expression      { Op("+", e1, e2) }
  | e1=expression TIMES e2=expression     { Op("*", e1, e2) }
  | e1=expression MINUS e2=expression     { Op("-", e1, e2) }
  | e1=expression OR e2=expression     { Op("||", e1, e2) }
  | e1=expression AND e2=expression     { Op("&&", e1, e2) }
  | e1=expression EQ e2=expression     { Op("=", e1, e2) }
  | IF e1=expression THEN e2=expression ELSE e3=expression { If(e1,e2,e3) }
  | LET e1=VAR EQ e2=expression IN e3=expression { Let(e1,e2,e3, false) }
  | LET REC e1=VAR EQ e2=expression IN e3=expression { Let(e1,e2,e3, true) }
  | FUN args=VAR+ ARROW e=expression { List.fold_right (fun x acc -> Fun(x,acc)) args e}
  | MINUS e=expression                    { Op("-", Int 0, e) } (* le moins unaire *)
  | k=applic                          { k }
  | LPAREN RPAREN                    { Unit }
  | BEGIN END                             { Unit }
  | BANG s=VAR                { Bang(s) }
  | s=VAR ASSIGN e=expression                { Assign(s, e) }
  | BEGIN e=expression END                { e }
  | LPAREN e=expression RPAREN            { e } 
  | e1 = expression SEQ e2 = expression { Seq(e1, e2) } 
  (*TODO : a tuple is a tuple even without parentheses*)
  | LPAREN xs=expr_list COMMA x=expression RPAREN            { Tuple (xs @ [x]) } 

expr_list:
  | x=expression                        { [x] }
  | xs=expr_list COMMA x=expression     { xs @ [x] }

applic:
  | e1=applic e2=sexpr {App(e1, e2)}
  | e1=sexpr e2=sexpr {App(e1, e2)}

sexpr:
  | LPAREN e=expression RPAREN {e}
  | i=INT {Int i}
  | b=BOOL {Bool b}
  | BANG s=VAR { Bang(s) }
  | s=VAR {String s}