exception UnalignedAccess of int32
type chunk =
    ChunkEmpty
  | ChunkFull of
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  | ChunkIO of (int -> int32) * (int -> int32 -> unit)
val splitaddress : int32 -> int * int
val newchunk :
  unit -> (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
val mask : int32
class memory :
  object
    val memory : chunk array
    method readByte : int32 -> int32
    method readHalfword : int32 -> int32
    method readWord : int32 -> int32
    method writeByte : int32 -> int32 -> unit
    method writeHalfword : int32 -> int32 -> unit
    method writeWord : int32 -> int32 -> unit
  end
