val mylsl : int32 -> int -> int32
val insertbits : int32 -> int -> int -> int32 -> int32
val truth : int32 -> int -> bool
val extractbits : int32 -> int -> int -> bool -> int32
val bits : int32 -> int -> int -> int32
val latch : 'a ref -> 'a -> 'a
val ( ++ ) : 'a -> ('a -> 'b) -> 'b
val uDiv : int32 -> int32 -> int32
val uRem : int32 -> int32 -> int32
val from_to : (int -> 'a -> 'a) -> 'a -> int -> int -> 'a
