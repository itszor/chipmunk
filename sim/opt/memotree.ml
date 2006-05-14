type memo = {
  reduce: Ast.ast matchtype;
  astequiv: Ast.ast;
  cost: int;
  asm: asmbinding;
  children: memo list
}

(* Memotree mirrors structure of ast, without actually being a part of it *)
and memotree = MTri of memotree * memotree * memotree
             | MBin of memotree * memotree
             | MUn of memotree
             | MNull
             | MMatch of memo * memotree
             | MVec of memotree list

and asmbinding = (string, Ast.ast) Hashtbl.t ->
  Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Iformat.format list

and 'a matchtype = IntNode of 'a
                 | FloatNode of 'a
                 | NoMatch

let rec string_of_memotree m =
  match m with
    MTri(m1,m2,m3) -> Printf.sprintf "(m3 %s %s %s)"
      (string_of_memotree m1)
      (string_of_memotree m2)
      (string_of_memotree m3)
  | MBin(m1,m2) -> Printf.sprintf "(m2 %s %s)"
      (string_of_memotree m1)
      (string_of_memotree m2)
  | MUn(m1) -> Printf.sprintf "(m1 %s)"
      (string_of_memotree m1)
  | MMatch(c,d) -> Printf.sprintf "(#%d %s)"
      c.cost
      (string_of_memotree d)
  | MNull -> "()"

let iter fn tree =
  let rec iter' tree =
    fn tree;
    match tree with
      MTri(a,b,c) -> iter' a; iter' b; iter' c
    | MBin(a,b) -> iter' a; iter' b
    | MUn(a) -> iter' a
    | MMatch(a,b) -> iter' b
    | MNull -> ()
  in
    iter' tree

let map fn tree =
  let rec map' tree =
    let walked =
      match tree with
        MTri(a,b,c) -> MTri(map' a, map' b, map' c)
      | MBin(a,b) -> MBin(map' a, map' b)
      | MUn(a) -> MUn(map' a)
      | MMatch(a,b) -> MMatch(a, map' b)
      | MNull -> tree
    in
      fn walked
  in
    map' tree

(* Iterates post-order(?) over each element in the tree, from bottom-left. Eg:
 *    3
 *   / \
 *  1   2
 * Nodes are visited in that order.
 *)
let fold_left fn acc tree =
  let rec fold_left' acc tree =
    match tree with
      MTri(a,b,c) ->
        let m1 = fn (fold_left' acc a) a in
        let m2 = fn (fold_left' m1 b) b in
        fn (fold_left' m2 c) c
    | MBin(a,b) ->
        let m1 = fn (fold_left' acc a) a in
        fn (fold_left' m1 b) b
    | MUn(a) ->
        fn (fold_left' acc a) a
    | MMatch(_, b) ->
        fn (fold_left' acc b) b
    | MNull -> acc
  in
    fold_left' acc tree

(* Same, but right-to-left, eg:
       3
      / \
     2   1
*)
let fold_right fn tree acc =
  let rec fold_right' tree acc =
    match tree with
      MTri(a,b,c) ->
        let m1 = fn c (fold_right' c acc) in
        let m2 = fn b (fold_right' b m1) in
        fn a (fold_right' a m2)
    | MBin(a,b) ->
        let m1 = fn b (fold_right' b acc) in
        fn a (fold_right' a m1)
    | MUn(a) ->
        fn a (fold_right' a acc)
    | MMatch(_, b) ->
        fn b (fold_right' b acc)
    | MNull -> acc
  in
    fold_right' tree acc
