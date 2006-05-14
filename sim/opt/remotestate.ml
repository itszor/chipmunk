open Common
open I32op
open Index

let makenewhdr oldidx oldhdr newhdr newblkaddr =
  newhdr.parent <- oldhdr.parent;
  newhdr.count <- oldhdr.count;
  newhdr.meta <- oldhdr.meta;
  oldhdr.offset <- newblkaddr;
  oldhdr.parent <- oldidx;
  oldhdr.count <- Int32.zero;
  oldhdr.meta <- Int32.zero

class ['a] remotestate initblocks (memzone:'a) =
  object(self)
  
  val mutable initialblocks = initblocks
  val mutable hdrptr = initblocks
  val blkzone = memzone

  initializer
    blkzone#clear
  
  method newhdr =
    let p = hdrptr in
    hdrptr <- hdrptr +! !!1;
    p
  
  method replaceblock (oc:int) (oldref:int) (words:int32 list) =
    let size = List.length words in
    let replbase = blkzone#allocate size in
    ()
  
  end
