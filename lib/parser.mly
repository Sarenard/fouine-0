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
%token LET IF THEN ELSE IN FUN FUNCTION ARROW REC
%token L LE G GE NE EQ
%token EOF
%token TRY E RAISE
%token <int> INT       /* le lexème INT a un attribut entier */
%token <bool> BOOL
%token <string> VAR
%token LST_PREPEND LST_CONCAT LBRACKET RBRACKET

/* PARTIE 3, on donne les associativités et on classe les priorités *********** */
/* priorité plus grande sur une ligne située plus bas */
%nonassoc ELSE IN ARROW  
%nonassoc LE GE NE
%nonassoc L G
%nonassoc below_PIPE
%left PIPE
%left EQ
%right AND
%right OR
%right LST_CONCAT
%right LST_PREPEND
%left PLUS MINUS
%left TIMES DIV
%nonassoc below_COMMA
%left COMMA
%right SEQ

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
  | e1=expression SEQ e2=expression { Seq(e1, e2) }
  | e=tuple_expr { e }

tuple_expr:
  | e=assign_expr %prec below_COMMA { e }
  | e1=assign_expr COMMA e2=assign_expr rest=tuple_more
      { Tuple (e1 :: e2 :: rest) }

tuple_more:
  | %prec below_COMMA { [] }
  | COMMA e=assign_expr rest=tuple_more { e :: rest }

assign_expr:
  | e1=operator ASSIGN e2=assign_expr { App(App(String ":=", e1), e2) }
  | e=controwlflow { e }

controwlflow:
  | IF e1=expression THEN e2=expression ELSE e3=expression { If(e1,e2,e3) }

  (*normal let*)
  | LET e1=pattern EQ e2=expression IN e3=expression { Let(e1, e2, e3, false) }
  | LET REC e1=pattern EQ e2=expression IN e3=expression { Let(e1, e2, e3, true) }

  (*let f x = ...*)
  | LET s=VAR e2=pattern+ EQ e3=expression IN e4=expression { Let(PVar s, (List.fold_right (fun x acc -> Fun(x, acc)) e2 e3), e4, false) }
  | LET REC s=VAR e2=pattern+ EQ e3=expression IN e4=expression { Let(PVar s, (List.fold_right (fun x acc -> Fun(x, acc)) e2 e3), e4, true) }

  | FUN args=pattern+ ARROW e=expression { List.fold_right (fun x acc -> Fun(x,acc)) args e}
  | MATCH e=expression WITH m=match_inner {Match(e, m)}
  | FUNCTION m=match_inner { Fun(PVar "", Match(String "", m)) }
  | TRY e1=expression WITH m=match_inner {Try(e1, m)}
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
  | e1 = operator LST_PREPEND e2 = operator { Op("::", e1, e2) }
  | e1 = operator LST_CONCAT e2 = operator { Op("@", e1, e2) }
  | MINUS e=operator { Op("-", Int 0, e)}
  | e = applic { e }

applic:
  | e1=applic e2=expr_ident {App(e1, e2)}
  | expr_ident {$1}

expr_ident:
  | i=INT {Int i}
  | b=BOOL {Bool b}
  | RAISE LPAREN E e=expression RPAREN {Raise e}
  | BANG e=expr_ident { App(String "!", e) }
  | s=VAR {String s}
  | LBRACKET RBRACKET { LinkedList [] }
  | LBRACKET e=expr_list_list RBRACKET { LinkedList e }
  | LPAREN RPAREN                    { Tuple [] }
  | LPAREN e=expression RPAREN {e}
  | BEGIN END                             { Tuple [] }
  | BEGIN e=expression END                { e }

expr_list_list:
  | x=tuple_expr                      { [x] }
  | xs=expr_list_list SEQ x=tuple_expr { xs @ [x] }

pattern:
  | p=pattern_tuple { p }

pattern_atom:
  | i=INT { PInt i }
  | b=BOOL { PBool b }
  | s=VAR { PVar s }
  | LBRACKET RBRACKET { PNil }
  | E p=pattern_atom { PE p } 
  | LPAREN RPAREN { PTuple [] }
  | LPAREN p=pattern RPAREN { p }

pattern_consable:
  | p1=pattern_atom LST_PREPEND p2=pattern_consable { PCons(p1, p2) }
  | p=pattern_atom { p }

pattern_tuple:
  | p=pattern_consable { p }
  | p1=pattern_consable COMMA p2=pattern_consable rest=pattern_more
      { PTuple (p1 :: p2 :: rest) }

pattern_more:
  | { [] }
  | COMMA p=pattern_consable rest=pattern_more { p :: rest }