%{

  open Lexing
  open Mml

%}

%token PLUS STAR LPAR RPAR DOT NOT MINUS IF THEN ELSE FUN RARROW LARROW COLON LET REC SEMICOLON IN EQUAL TYPE LBRACKET RBRACKET MUTABLE SLASH DEQUAL OR MOD INFERIOR IEQUAL NEQUAL AND UNIT INT
%token <int> CST
%token <string> IDENT
%token <bool> BOOL
%token EOF

%left PLUS
%left STAR

%start program
%type <Mml.prog> program

%%

program:
| code=expression EOF { {types=[]; code} }
| error
  { let pos = $startpos in
    let message = Printf.sprintf
      "echec a la position %d, %d"
      pos.pos_lnum 
      (pos.pos_cnum - pos.pos_bol)
    in
    failwith message }
;

simple_expression:
| n=CST { Int(n) }
| b=BOOL { Bool(b) }
| UNIT { Unit }
| s=IDENT { Var(s) }
| s_e=simple_expression DOT s=IDENT { GetF(s_e, s) }
| LPAR e=expression RPAR { e }
;

expression:
| e=simple_expression { e }
| op=unop e1=expression { Uop(op, e1) }
| e1=expression op=binop e2=expression { Bop(op, e1, e2) }
| e=expression s_e=simple_expression { App(e, s_e) }
| IF e1=expression THEN e2=expression { If(e1, e2, Unit) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If(e1, e2, e3) }
| FUN LPAR s=IDENT COLON t=typ RPAR RARROW e=expression { Fun(s, t, e) }
| LET s=IDENT list(let_args {} ) EQUAL e1=expression IN e2=expression { Let(s, e1, e2) }
| LET REC s=IDENT list(let_args {}) COLON typ EQUAL e1=expression IN e2=expression { Let(s, e1, e2) }
| s_e=simple_expression DOT s=IDENT LARROW e=expression { SetF(s_e, s, e) }
| e1=expression SEMICOLON e2=expression { Seq(e1, e2) }
;

let_args: LPAR IDENT COLON typ RPAR {}

type_def: TYPE s1=IDENT EQUAL LBRACKET type_args list(type_args) RBRACKET {};

type_args: option(MUTABLE) s2=IDENT COLON t=typ SEMICOLON {};

typ:
| INT { TInt }
| BOOL { TBool }
| UNIT { TUnit }
| s=IDENT { TStrct(s) }
| t1=typ RARROW t2=typ { TFun(t1, t2) }
| LPAR t=typ RPAR { t }
;


%inline unop:
| MINUS { Neg }
| NOT { Not }
;

%inline binop:
| PLUS { Add }
| STAR { Mul }
| MINUS { Sub }
| SLASH { Div }
| MOD { Mod }
| DEQUAL { Eq }
| NEQUAL { Neq }
| INFERIOR { Lt }
| IEQUAL { Le }
| AND { And }
| OR { Or }
;

