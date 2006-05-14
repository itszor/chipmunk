type index = {
  mutable offset : int32;
  mutable parent : int32;
  mutable meta : int32;
  mutable count : int32;
}
exception BadList
val index_of_list : int32 list -> index
val index_to_list : index -> int32 list
val getindex : Memory.memory -> Processor.processor -> int32 -> index
val setindex : Memory.memory -> Processor.processor -> int32 -> index -> unit
class indexer : int32 ->
object
  method setbase : int32 -> unit
  method alloc : int32
  method clear : unit
end
