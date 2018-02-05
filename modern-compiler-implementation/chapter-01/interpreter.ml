open Grammar


let rec maxargs : stm -> int = function
  | CompoundStm (stm1, stm2) ->
    max (maxargs stm1) (maxargs stm2)
  | PrintStm ls ->
    max (List.length ls)
        (List.fold_left max 0 (List.map maxargs' ls))
  | AssignStm (_, exp) ->
    maxargs' exp

and maxargs' : exp -> int = function
  | EseqExp (stm, exp) ->
    max (maxargs stm) (maxargs' exp)
  | OpExp (exp1, _, exp2) ->
    max (maxargs' exp1) (maxargs' exp2)
  | NumExp _ | IdExp _ ->
    0


let fn_of_op = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Div -> ( / )

let interp' print stm =
  let rec interp_stm binds : stm -> (string * int) list = function
    | CompoundStm (stm1, stm2) ->
      let binds = interp_stm binds stm1 in
      interp_stm binds stm2
    | AssignStm (id, exp) ->
      let (v, binds) = interp_exp binds exp in
      (id, v) :: binds
    | PrintStm exps ->
      let folder binds exp =
        let (v, binds) = interp_exp binds exp in
        print (string_of_int v);
        binds in
      List.fold_left folder binds exps
  and interp_exp binds : exp -> (int * (string * int) list) = function
    | IdExp id ->
      (List.assoc id binds, binds)
    | NumExp n ->
      (n, binds)
    | OpExp (n1, op, n2) ->
      let (val1, binds) = interp_exp binds n1 in
      let (val2, binds) = interp_exp binds n2 in
      let fn = fn_of_op op in
      (fn val1 val2, binds)
    | EseqExp (stm, exp) ->
      let binds = interp_stm binds stm in
      let (v, binds) = interp_exp binds exp in
      (v, binds)
  in
  let _ = interp_stm [] stm in
  ()


let interp =
  interp' print_endline
