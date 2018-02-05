{
type tokens =
  | WHILE | FOR | TO | BREAK | LET | IN | END
  | IF | THEN | ELSE | DO | OF | NIL
  | FUNCTION | VAR | TYPE | ARRAY
  | LPAREN | RPAREN
  | LBRACE | RBRACE
  | LBRACK | RBRACK
  | COMMA | COLON | SEMICOLON | DOT
  | PLUS | MINUS | STAR | SLASH
  | EQ | NEQ | LT | LE | GT | GE
  | AMP | BAR | ASSIGN
  | STRING of string
  | INT of int
  | ID of string
  | EOF

type pos =
  { mutable line : int
  ; mutable line_start : int }

let curr = { line = 1; line_start = 0 }

let fail (lexbuf : Lexing.lexbuf) msg =
  let col = lexbuf.lex_curr_pos - curr.line_start in
  let pos = Printf.sprintf "line %d, character %d: " curr.line col in
  failwith (pos ^ msg)
}

let id =
  ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let caret_escaped =
  ['@' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'Z' '[']


rule read = parse
| eof  { EOF }

| [' ' '\r' '\t']
  { read lexbuf }

| '\n'
  { curr.line <- curr.line + 1;
    curr.line_start <- lexbuf.lex_curr_pos + 1;
    read lexbuf }

| "/*"  { comment 1 lexbuf }

| "*/"  { fail lexbuf "Unbalanced comment" }

| "while"     { WHILE }   | "for"       { FOR }
| "to"        { TO }      | "break"     { BREAK }
| "let"       { LET }     | "in"        { IN }
| "end"       { END }     | "if"        { IF }
| "then"      { THEN }    | "else"      { ELSE }
| "do"        { DO }      | "of"        { OF }
| "nil"       { NIL }     | "function"  { FUNCTION }
| "var"       { VAR }     | "type"      { TYPE }
| "array"     { ARRAY }

| '(' { LPAREN } | ')' { RPAREN }
| '[' { LBRACK } | ']' { RBRACK }
| '{' { LBRACE } | '}' { RBRACE }
| '.' { DOT }       | ',' { COMMA }
| ':' { SEMICOLON } | ';' { COLON }

| '+'   { PLUS } | '-'   { MINUS }
| '*'   { STAR } | '/'   { SLASH }
| '='   { EQ } | "<>"  { NEQ }
| '<'   { LT } | '>'   { GT }
| "<="  { LE } | ">="  { GE }
| '&'   { AMP } | '|'   { BAR }
| ":="  { ASSIGN }

| id  { ID (Lexing.lexeme lexbuf) }
| ['0'-'9']+
  { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '"' { string (Buffer.create 16) lexbuf }
| _   { fail lexbuf ("Unexpected token: " ^ Lexing.lexeme lexbuf) }


and comment depth = parse
| "/*"  { comment (depth + 1) lexbuf }
| "*/"  { if depth <= 1 then
            read lexbuf
          else
            comment (depth - 1) lexbuf }
| '\n'  { curr.line <- curr.line + 1;
          curr.line_start <- lexbuf.lex_curr_pos + 1;
          comment depth lexbuf }
| eof   { fail lexbuf "Unterminated comment" }
| _     { comment depth lexbuf }


and string buf = parse
| '"'     { STRING (Buffer.contents buf) }
| '\\' (['n' 't' '"' '\\'] as c)
          { Buffer.add_char buf c;
            string buf lexbuf }
| "\\^" (caret_escaped as c)
          { let chr = Char.chr (Char.code c - 64) in
            Buffer.add_char buf chr;
            string buf lexbuf }
| '\\' (['0'-'9']['0'-'9']['0'-'9'] as n)
          { let chr = int_of_string n in
            if chr > 128 || chr < 0 then
              fail lexbuf ("Invalid character code: " ^ string_of_int chr)
            else
              Buffer.add_char buf (Char.chr chr);
              string buf lexbuf }
| '\\'
| '\\' [' ' '\t' '\n' '\r']* '\\'
          { string buf lexbuf }
| ['\032'-'\126'] as c
          { Buffer.add_char buf c;
            string buf lexbuf }
| eof     { fail lexbuf "Unterminated string" }
| _       { fail lexbuf "Unexpected character in string" }


and string_esc str = parse
| '\\'    { string str lexbuf }
| '\n'    { curr.line <- curr.line + 1;
            curr.line_start <- lexbuf.lex_curr_pos + 1;
            string_esc str lexbuf }
| [' ' '\t' '\r']*
          { string_esc str lexbuf }
| eof     { fail lexbuf "Unterminated string escape" }
| _ as c  { fail lexbuf ("Invalid character in string escape: " ^ Char.escaped c) }
