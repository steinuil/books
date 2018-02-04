let prog =
  let open Grammar in
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
        EseqExp(PrintStm[IdExp"a";OpExp(IdExp"a", Minus,NumExp 1)],
             OpExp(NumExp 10, Times, IdExp"a"))),
     PrintStm[IdExp "b"]))


let prog2 =
  "a := 5 + 3; b := ( print ( a, a - 1) , 10 * a ) ; print ( b )"


let () =
  let m = Interpreter.maxargs prog in
  print_string "Maximum number of arguments to print: ";
  print_int m;
  print_newline ();
  print_endline "Evaluating the AST:";
  Interpreter.interp prog;
  print_endline "Parsing and evaluating the string:";
  let program = Parser.program Lexer.read (Lexing.from_string prog2) in
  Interpreter.interp program
