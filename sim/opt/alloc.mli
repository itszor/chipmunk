exception AllocationError
class zone :
  int32 ->
  int32 ->
  object
    val base : int32
    val limit : int32
    val mutable ptr : int32
    method allocate : int32 -> int32
    method clear : unit
  end
