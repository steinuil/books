%{
%}

%token WHILE FOR TO BREAK LET IN END IF THEN ELSE DO OF NIL
%token FUNCTION VAR TYPE ARRAY
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token COMMA COLON SEMICOLON DOT
%token PLUS MINUS STAR SLASH
%token EQ NEQ LT LE GT GE
%token AMP BAR ASSIGN
%token <string> STRING ID
%token <int> INT
%token EOF

%nonassoc THEN DO
%nonassoc ELSE OF
%nonassoc EQ NEQ LT GT LE GE
%left AMP BAR
%left PLUS MINUS
%left STAR SLASH

%start <unit> program

%%

program:
| EOF
  {}
| expression; EOF
  {}


binop:
| expression PLUS expression
  {}
| expression MINUS expression
  {}
| expression STAR expression
  {}
| expression SLASH expression
  {}
| expression EQ expression
  {}
| expression NEQ expression
  {}
| expression LT expression
  {}
| expression LE expression
  {}
| expression GT expression
  {}
| expression GE expression
  {}
| expression AMP expression
  {}
| expression BAR expression
  {}


expression:
| NIL
  {}
| LPAREN RPAREN
  {}
| lvalue
  {}
| STRING
  {}
| INT
  {}
| MINUS expression
  {}
| LET declaration+ IN separated_list(COMMA, expression) END
  {}
| IF expression THEN expression
  {}
| IF expression THEN expression ELSE expression
  {}
| binop
  {}
| ID LPAREN separated_list(COMMA, expression) RPAREN
  {}
| LPAREN separated_nonempty_list(SEMICOLON, expression) RPAREN
  {}
| WHILE expression DO expression
  {}
| FOR ID ASSIGN expression TO expression DO expression
  {}
| BREAK
  {}
| ID LBRACE separated_list(COMMA, rec_field) RBRACE
  {}
| ID LBRACK expression RBRACK OF expression
  {}


rec_field:
| ID EQ expression
  {}


lvalue:
| ID
  {}
| lvalue DOT ID
  {}
| lvalue LBRACK expression RBRACK
  {}


declaration:
| FUNCTION ID LPAREN separated_list(COMMA, tyfield) RPAREN EQ expression
  {}
| FUNCTION ID LPAREN separated_list(COMMA, tyfield) RPAREN COLON ID EQ expression
  {}
| VAR ID ASSIGN expression
  {}
| VAR ID COLON ID ASSIGN expression
  {}
| TYPE ID EQ type_
  {}


type_:
| ID
  {}
| LBRACE separated_list(COMMA, tyfield) RBRACE
  {}
| ARRAY OF ID
  {}


tyfield:
| ID COLON ID
  {}
