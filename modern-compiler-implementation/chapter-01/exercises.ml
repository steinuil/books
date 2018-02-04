(* Exercise 1.1 *)

type key = string
type tree = Leaf | Tree of tree * key * tree

let empty = Leaf

let rec insert tree key = match tree with
  | Leaf -> Tree (Leaf, key, Leaf)
  | Tree (l, k, r) ->
    if key < k then
      Tree (insert l key, k, r)
    else if key > k then
      Tree (l, k, insert r key)
    else
      Tree (l, key, r)


(* Exercise a. *)
let rec member tree key = match tree with
  | Leaf -> false
  | Tree (l, k, r) ->
    if key = k then
      true
    else
      member l key || member r key


(* Exercise b. *)
module B = struct
  type key = string
  type 'a tree = Leaf | Tree of 'a tree * (key * 'a) * 'a tree

  let empty = Leaf


  let rec insert tree (key, bind) = match tree with
    | Leaf -> Tree (Leaf, (key, bind), Leaf)
    | Tree (l, (k, v), r) ->
      if key < k then
        Tree (insert l (key, bind), (k, v), r)
      else if key > k then
        Tree (l, (k, v), insert r (key, bind))
      else
        Tree (l, (key, bind), r)


  let rec lookup tree key = match tree with
    | Leaf -> None
    | Tree (l, (k, v), r) ->
      if key = k then
        v
      else
        match lookup l key with
        | Some v -> Some v
        | None -> lookup r key
end


(* Exercise c. *)
(* From the toplevel:

 # let seq1 = ["t"; "s"; "p"; "i"; "p"; "f"; "b"; "s"; "t"];;
 # List.fold_left insert empty seq1;;
 * Tree
    (Tree
      (Tree (Tree (Tree (Tree (Leaf, "b", Leaf), "f", Leaf), "i", Leaf), "p",
        Leaf),
      "s", Leaf),
    "t", Leaf)

 # let seq2 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"];;
 # List.fold_left insert empty seq2;;
 * Tree (Leaf, "a",
    Tree (Leaf, "b",
     Tree (Leaf, "c",
      Tree (Leaf, "d",
       Tree (Leaf, "e",
        Tree (Leaf, "f",
         Tree (Leaf, "g", Tree (Leaf, "h", Tree (Leaf, "i", Leaf)))))))))
 *)


(* Exercise d. *)
(* Hell no I'm not gonna read that whole book *)
