open Lazy

type 'a seq = Cons of 'a * ('a seq Lazy.t)
            | Empty

exception Lazy_empty

let hd = function
    Empty -> raise Lazy_empty
  | Cons(l,ls) -> l

let tl = function
    Empty -> raise Lazy_empty
  | Cons(l,ls) -> force ls

let rec ints a =
  Cons(a, lazy (ints (a+1)))

let rec of_list x =
  match x with
    [] -> Empty
  | l::ls -> Cons(l, lazy (of_list ls))

let rec append a b =
  match a with
    Empty -> b
  | Cons(l,ls) -> Cons(l, lazy (append (force ls) b))

let cons a b =
  Cons(a, lazy b)

let rec to_list x =
  match x with
    Empty -> []
  | Cons(l,ls) -> l :: to_list (force ls)

let rec take n seq =
  match n with
    0 -> []
  | n -> hd seq :: (take (n-1) (tl seq))

let rec but n seq =
  match n with
    0 -> seq
  | n -> but (n-1) (tl seq)

let rec map fn seq =
  match seq with
    Empty -> Empty
  | Cons(l,ls) -> Cons(fn l, lazy (map fn (force ls)))

let rec fold_left fn acc seq =
  match seq with
    Empty -> acc
  | Cons(l,ls) -> fold_left fn (fn acc l) (force ls)

let rec fold_right fn seq acc =
  match seq with
    Empty -> acc
  | Cons(l,ls) -> fn l (fold_right fn (force ls) acc)
