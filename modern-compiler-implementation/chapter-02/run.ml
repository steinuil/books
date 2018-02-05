let string_of_token =
  let open Tiger in function
  | WHILE -> "WHILE" | FOR -> "FOR"
  | TO -> "TO" | BREAK -> "BREAK" | LET -> "LET"
  | IN -> "IN" | END -> "END"
  | IF -> "IF" | THEN -> "THEN" | ELSE -> "ELSE"
  | DO -> "DO" | OF -> "OF" | NIL -> "NIL"
  | FUNCTION -> "FUNCTION" | VAR -> "VAR"
  | TYPE -> "TYPE" | ARRAY -> "ARRAY"
  | LPAREN -> "LPAREN" | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE" | RBRACE -> "RBRACE"
  | LBRACK -> "LBRACK" | RBRACK -> "RBRACK"
  | COMMA -> "COMMA" | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON" | DOT -> "DOT"
  | PLUS -> "PLUS" | MINUS -> "MINUS"
  | STAR -> "STAR" | SLASH -> "SLASH"
  | EQ -> "EQ" | NEQ -> "NEQ" | LT -> "LT"
  | LE -> "LE" | GT -> "GT" | GE -> "GE"
  | AMP -> "AMP" | BAR -> "BAR" | ASSIGN -> "ASSIGN"
  | STRING str -> "STRING(\"" ^ str ^ "\")"
  | INT n -> "NUM(" ^ string_of_int n ^ ")"
  | ID id -> "ID(" ^ id ^ ")"
  | EOF -> "EOF"


let string_of_tokens lexbuf =
  let rec loop acc =
    match Tiger.read lexbuf with
    | EOF -> (acc ^ "EOF")
    | tok ->
      loop (acc ^ string_of_token tok ^ " ")
  in
  loop ""


let test_files =
  List.tl @@ Array.to_list Sys.argv


let () =
  test_files |> List.iter begin fun f ->
    let chan = open_in f in
    let lexbuf = Lexing.from_channel chan in
    print_endline (f ^ ":");
    print_endline (string_of_tokens lexbuf)
  end

