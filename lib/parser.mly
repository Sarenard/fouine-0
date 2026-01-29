%{
open Expr
%}

/* PARTIE 2, on liste les lexèmes (lien avec le fichier lexer.mll) ******* */                                   
%token EQ
%token OR AND
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token LET IF THEN ELSE IN FUN ARROW
%token PRINT
%token EOL             /* EOL = End Of Line, retour à la ligne */
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR

/* PARTIE 3, on donne les associativités et on classe les priorités *********** */
/* priorité plus grande sur une ligne située plus bas */
%nonassoc ELSE IN ARROW (*should print be right-associative ?*)
%left OR
%left AND
%left PLUS MINUS
%left TIMES
%left PRINT

/* PARTIE 4, le point d'entrée ******************************************* */
%start main             /* "start" signale le point d'entrée du parser: */
                        /* c'est ici le non-terminal "main", qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée "main" */

/* PARTIE 5 : la grammaire, enfin ! ************************************** */                                                         
%%

main:                       /* <- le point d'entrée (cf. + haut, "start") */
e=expression EOL { e }  /* on reconnaît une expression suivie de "EndOfLine", on la renvoie telle quelle */


/* règles de grammaire pour les expressions ; le non-terminal s'appelle "expression" */                                                                                
expression:			   
   /* on appelle i l'attribut associé à INT */
  | i=INT                             { Int i }
  | b=BOOL                             { Bool b }
  | e1=expression PLUS e2=expression      { Add(e1,e2) }
  | e1=expression TIMES e2=expression     { Mul(e1,e2) }
  | e1=expression MINUS e2=expression     { Min(e1,e2) }
  | e1=expression OR e2=expression     { Or(e1,e2) }
  | e1=expression AND e2=expression     { And(e1,e2) }
  | IF e1=expression THEN e2=expression ELSE e3=expression { If(e1,e2,e3) }
  | LET e1=VAR EQ e2=expression IN e3=expression { Let(e1,e2,e3) }
  | FUN x=VAR ARROW e=expression { Fun(x,e) }
  | MINUS e=expression                    { Min(Int 0, e) } (* le moins unaire *)
  | PRINT e=expression                    { PrInt(e) } (* le moins unaire *)
  | LPAREN e=expression RPAREN            { e } 
  | s=VAR                             { String s }


