(* Lay out a set of block indices. Input is a block tag dynarray, which is
   modified in-place by filling "writeidx" field, an index allocator, and
   a single block ref to use as a hook point.
*)

let layout vertices indexer start_at replace =
  let len = DynArray.length vertices in
  for i=0 to len-1 do
    let tag = DynArray.get vertices i in
    if i=start_at then
      tag.Block.writeidx <- Some replace
    else
      let newidx = indexer#alloc in
      tag.Block.writeidx <- Some newidx
  done
