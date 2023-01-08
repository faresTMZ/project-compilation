%{

  open Lexing
  open Mml
  open Option

%}

%token PLUS STAR NOT MINUS EQUAL SLASH DEQUAL OR MOD INFERIOR IEQUAL NEQUAL AND
%token LPAR RPAR DOT RARROW LARROW COLON SEMICOLON LBRACKET RBRACKET
%token IF THEN ELSE LET REC IN FUN TYPE MUTABLE T_UNIT T_INT T_BOOL PAR
%token <int> CST
%token <string> IDENT
%token <bool> BOOL
%token EOF

%left CST BOOL
%left SEMICOLON LPAR LBRACKET IDENT IN THEN RARROW LARROW PAR
%left ELSE
%left NEQUAL INFERIOR IEQUAL DEQUAL
%left MOD
%left PLUS MINUS OR
%left STAR SLASH AND
%nonassoc NOT

%start program
%type <Mml.prog> program

%%

program:
| typesL=list(type_def) code=expression EOF { {types=typesL; code} }
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
| PAR { Unit }
| s=IDENT { Var(s) }
| s_e=simple_expression DOT s=IDENT { GetF(s_e, s) }
| LBRACKET lH=struct_args lT=list(struct_args) RBRACKET { Strct (lH :: lT) }
| LPAR e=expression RPAR { e }
;

expression:
| e=simple_expression { e }
| op=unop e1=expression { Uop(op, e1) }
| e1=expression op=binop e2=expression { Bop (op, e1, e2) }
| e=expression s_e=simple_expression { App (e, s_e) }
| IF e1=expression THEN e2=expression { If (e1, e2, Unit) }
| IF e1=expression THEN e2=expression ELSE e3=expression { If (e1, e2, e3) }
| FUN LPAR s=IDENT COLON t=typ RPAR RARROW e=expression { Fun (s, t, e) }
| LET s=IDENT list(let_args {} ) EQUAL e1=expression IN e2=expression { Let (s, e1, e2) }
| LET REC s=IDENT list(let_args {}) COLON typ EQUAL e1=expression IN e2=expression { Let (s, e1, e2) }
| s_e=simple_expression DOT s=IDENT LARROW e=expression { SetF (s_e, s, e) }
| e1=expression SEMICOLON e2=expression { Seq (e1, e2) }
;

let_args: LPAR IDENT COLON typ RPAR {};

type_def: TYPE s1=IDENT EQUAL LBRACKET lH=type_args lT=list(type_args) RBRACKET { s1, lH::lT};

type_args: m=option(MUTABLE) s2=IDENT COLON t=typ SEMICOLON { s2, t, is_some(m) };

struct_args: i=IDENT EQUAL e=expression SEMICOLON { i, e };

typ:
| T_INT { TInt }
| T_BOOL { TBool }
| T_UNIT { TUnit }
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

