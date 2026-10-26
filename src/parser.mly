%{
  open Ast
%}

%token <int> NUM
%token <bool> BOOL
%token <string> ID
%token UNIT_VAL
%token TINT TBOOL TUNIT TREF
%token IF THEN ELSE
%token LET IN
%token WHILE DO
%token NEW DEREF NOT ASG
%token PLUS MINUS TIMES DIV
%token LT GT EQ EQEQ
%token AND OR
%token COLON SEMI
%token LPAREN RPAREN LBRACE RBRACE
%token EOF

/* Precedencia e associatividade (da menor para a maior) */
%nonassoc IN
%right SEMI
%nonassoc DO ELSE
%nonassoc ASG
%left OR
%left AND
%left EQ EQEQ LT GT
%left PLUS MINUS
%left TIMES DIV
%nonassoc NOT DEREF NEW UMINUS
%nonassoc TREF

%start <Ast.ast> prog

%%

prog:
  | e = expr EOF { e }
;

tp:
  | TINT            { TInt }
  | TBOOL           { TBool }
  | TUNIT           { TUnit }
  | TREF t = tp     { TRef t }
  | LPAREN t = tp RPAREN { t }
;

expr:
  | n = NUM
      { Num n }
  | b = BOOL
      { Bool b }
  | UNIT_VAL
      { Unit }
  | LPAREN RPAREN
      { Unit }
  | x = ID
      { Id x }
  | LPAREN e = expr RPAREN
      { e }
  | LBRACE e = expr RBRACE
      { e }
  | MINUS e = expr %prec UMINUS
      { Binop (Sub, Num 0, e) }
  | NOT e = expr
      { Not e }
  | DEREF e = expr
      { Deref e }
  | NEW e = expr
      { New e }
  | e1 = expr PLUS e2 = expr
      { Binop (Sum, e1, e2) }
  | e1 = expr MINUS e2 = expr
      { Binop (Sub, e1, e2) }
  | e1 = expr TIMES e2 = expr
      { Binop (Mul, e1, e2) }
  | e1 = expr DIV e2 = expr
      { Binop (Div, e1, e2) }
  | e1 = expr LT e2 = expr
      { Binop (Lt, e1, e2) }
  | e1 = expr GT e2 = expr
      { Binop (Gt, e1, e2) }
  | e1 = expr EQEQ e2 = expr
      { Binop (Eq, e1, e2) }
  | e1 = expr EQ e2 = expr
      { Binop (Eq, e1, e2) }
  | e1 = expr AND e2 = expr
      { Binop (And, e1, e2) }
  | e1 = expr OR e2 = expr
      { Binop (Or, e1, e2) }
  | x = ID ASG e2 = expr
      { Asg (x, e2) }
  | e1 = expr SEMI e2 = expr
      { Seq (e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
      { If (e1, e2, e3) }
  | LET x = ID COLON t = tp EQ e1 = expr IN e2 = expr
      { Let (x, t, e1, e2) }
  | WHILE e1 = expr DO e2 = expr
      { Whi (e1, e2) }
;
