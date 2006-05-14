val makenewhdr : int32 -> Index.index -> Index.index -> int32 -> unit
class ['a] remotestate :
  int32 ->
  'a ->
  object
    constraint 'a = < allocate : int -> 'b; clear : unit; .. >
    val blkzone : 'a
    val mutable hdrptr : int32
    val mutable initialblocks : int32
    method newhdr : int32
    method replaceblock : int -> int -> int32 list -> unit
  end
