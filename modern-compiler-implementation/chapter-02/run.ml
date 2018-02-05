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
  [ "testcases/test1.tig"
  ; "testcases/test2.tig"
  ; "testcases/test3.tig"
  ; "testcases/test4.tig"
  ; "testcases/test5.tig"
  ; "testcases/test6.tig"
  ; "testcases/test7.tig"
  ; "testcases/test8.tig"
  ; "testcases/test9.tig"
  ; "testcases/test10.tig"
  ; "testcases/test11.tig"
  ; "testcases/test12.tig"
  ; "testcases/test13.tig"
  ; "testcases/test14.tig"
  ; "testcases/test15.tig"
  ; "testcases/test16.tig"
  ; "testcases/test17.tig"
  ; "testcases/test18.tig"
  ; "testcases/test19.tig"
  ; "testcases/test20.tig"
  ; "testcases/test21.tig"
  ; "testcases/test22.tig"
  ; "testcases/test23.tig"
  ; "testcases/test24.tig"
  ; "testcases/test25.tig"
  ; "testcases/test26.tig"
  ; "testcases/test27.tig"
  ; "testcases/test28.tig"
  ; "testcases/test29.tig"
  ; "testcases/test30.tig"
  ; "testcases/test31.tig"
  ; "testcases/test32.tig"
  ; "testcases/test33.tig"
  ; "testcases/test34.tig"
  ; "testcases/test35.tig"
  ; "testcases/test36.tig"
  ; "testcases/test37.tig"
  ; "testcases/test38.tig"
  ; "testcases/test39.tig"
  ; "testcases/test40.tig"
  ; "testcases/test41.tig"
  ; "testcases/test42.tig"
  ; "testcases/test43.tig"
  ; "testcases/test44.tig"
  ; "testcases/test45.tig"
  ; "testcases/test46.tig"
  ; "testcases/test47.tig"
  ; "testcases/test48.tig"
  ; "testcases/test49.tig"
  ; "testcases/merge.tig"
  ; "testcases/queens.tig"
  ]


let () =
  test_files |> List.iter begin fun f ->
    let chan = open_in f in
    let lexbuf = Lexing.from_channel chan in
    print_endline (f ^ ":");
    print_endline (string_of_tokens lexbuf)
  end

