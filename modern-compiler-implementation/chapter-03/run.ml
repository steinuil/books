let tiger_parse buf =
  Parser.program Lexer.read buf


open OUnit2


let test_parse f test_ctx =
  let chan = open_in f in
  try
    tiger_parse (Lexing.from_channel chan);
    close_in chan
  with exn ->
    close_in chan;
    raise exn


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



let suite =
  "Chapter 03" >:::
    (List.map (fun f -> ("Parsing " ^ f) >:: (test_parse f)) test_files)


let () =
  run_test_tt_main suite
