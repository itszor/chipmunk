open Memory
open I32op

type index = {
  mutable offset: int32;
  mutable parent: int32;
  mutable meta: int32;
  mutable count: int32
}

exception BadList

let index_of_list = function
    [a; b; c; d] -> { offset = a; parent = b; meta = c; count = d }
  | _ -> raise BadList

let index_to_list = function
    x -> [x.offset; x.parent; x.meta; x.count]

let getindex mem proc blk =
  let base = proc#getindirseg +! (blk *! 16l) in
  { offset = mem#readWord base;
    parent = mem#readWord (base +! 4l);
    meta = mem#readWord (base +! 8l);
    count = mem#readWord (base +! 12l) }

let setindex mem proc blk idx =
  let base = proc#getindirseg +! (blk *! 16l) in
  mem#writeWord base idx.offset;
  mem#writeWord (base +! 4l) idx.parent;
  mem#writeWord (base +! 8l) idx.meta;
  mem#writeWord (base +! 12l) idx.count

class indexer start =
  object (self)
  
  val mutable dynamic_base = start
  val mutable last = start
  
  method setbase b =
    dynamic_base <- b;
    last <- b
  
  method alloc =
    let prev = last in
    last <- Int32.succ last;
    prev

  method clear =
    last <- dynamic_base
  
  end
