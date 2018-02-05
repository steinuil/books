let prog =
  let open Grammar in
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
        EseqExp(PrintStm[IdExp"a";OpExp(IdExp"a", Minus,NumExp 1)],
             OpExp(NumExp 10, Times, IdExp"a"))),
     PrintStm[IdExp "b"]))


let prog2 =
  "a := 5 + 3; b := ( print ( a, a - 1 ) , 10 * a ) ; print ( b )"


open OUnit2


let test_maxargs prog mx test_ctx =
  let max' = Interpreter.maxargs prog in
  assert_equal max' mx


let test_interp prog test_ctx =
  let ls = ref [] in
  let print x = ls := x :: !ls in
  Interpreter.interp' print prog;
  assert_equal !ls ["80"; "7"; "8"]


let test_parser str test_ctx =
  let prog = Parser.program Lexer.read @@ Lexing.from_string str in
  test_interp prog test_ctx


let suite =
  "Chapter 01" >:::
    [ "maxargs" >:: (test_maxargs prog 2)
    ; "Interpreting the AST" >:: (test_interp prog)
    ; "Parsing and interpreting" >:: (test_parser prog2)
    ]


let () =
  run_test_tt_main suite
