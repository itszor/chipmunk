type 'a seq = Cons of 'a * ('a seq Lazy.t)
            | Empty

exception Lazy_empty

val hd : 'a seq -> 'a

val tl : 'a seq -> 'a seq

val of_list : 'a list -> 'a seq

val to_list : 'a seq -> 'a list

val append : 'a seq -> 'a seq -> 'a seq

val cons : 'a -> 'a seq -> 'a seq

val take : int -> 'a seq -> 'a list

val but : int -> 'a seq -> 'a seq

val map : ('a -> 'a) -> 'a seq -> 'a seq

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b seq -> 'a

val fold_right : ('a -> 'b -> 'b) -> 'a seq -> 'b -> 'b
