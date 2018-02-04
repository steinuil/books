%{
open Grammar
%}

%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMI COMMA ASSIGN
%token PRINT
%token <int> NUM
%token <string> ID
%token EOF

%left PLUS MINUS
%left TIMES DIV

%start <Grammar.stm> program

%%

program:
| EOF { failwith "empty program!" }
| s = statement; EOF
  { s }
| s = statement ; SEMI ; p = program
  { CompoundStm (s, p) }

statement:
| id = ID; ASSIGN; e = exp
  { AssignStm (id, e) }
| PRINT LPAREN e = separated_nonempty_list(COMMA, exp) RPAREN
  { PrintStm e }

%inline binop:
| PLUS  { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV   { Div }

exp:
| id = ID
  { IdExp id }
| n = NUM
  { NumExp n }
| e1 = exp; b = binop; e2 = exp
  { OpExp (e1, b, e2) }
| LPAREN s = statement; COMMA; e = exp RPAREN
  { EseqExp (s, e) }
