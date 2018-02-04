{ open Parser }


rule read = parse
| eof { EOF }

| [' ' '\n' '\r']
  { read lexbuf }

| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIV }

| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMI }
| ',' { COMMA }
| ":=" { ASSIGN }

| "print" { PRINT }

| ['1'-'9']['0'-'9']*
  { NUM (int_of_string @@ Lexing.lexeme lexbuf) }
| ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
  { ID (Lexing.lexeme lexbuf) }

| _
  { failwith "unexpected character" }
