
type code = Byte of int
          | Bytes of int list
          | Rel32 of int32
          | Rel8 of int32
          | Abs32 of int32
          | Abs8 of int32
          | Label of string

val split_int32 : int32 -> int32 list

val strint : int -> string

val flatten : code Seq.seq -> string
