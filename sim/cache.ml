open Bigarray
open State

let directmap elt linesize addr =
  [(Int32.to_int (Int32.div addr (Int32.of_int linesize))) mod elt]

let nway n elt linesize addr =
  let set = ref []
  and span = elt/n in
  for i = 0 to n-1 do
    set := (((Int32.to_int (Int32.div addr (Int32.of_int (linesize*4))))
             +i*span) mod elt) :: !set
  done;
  !set

let twoway = nway 2
let fourway = nway 4
let eightway = nway 8

exception Not_in_cache

exception Assoc_full

class cache state elt linesize assoc =
  object (self)
    val idx =
      Array1.create int32 c_layout elt

    val cache =
      Array1.create int32 c_layout (elt*linesize)
    
    val mutable latency = 3
    
    initializer
      self#flush
    
    method flush =
      for i = 0 to elt-1 do
        idx.{i} <- Int32.minus_one
      done
    
    method incache addr =
      List.exists (fun x -> idx.{x} == addr) (assoc addr)
    
    (* return a cache line starting from index, elt in length *)
    method readline index =
      let line = Array.make linesize Int32.zero in
      for i = 0 to linesize-1 do
        line.(i) <- cache.{index*linesize+i}
      done;
      line
    
    method writeline index line =
      for i = 0 to linesize-1 do
        cache.{index*linesize+i} <- line.(i)
      done
    
    (* base address (for cache tag) *)
    method baseaddr addr =
      Int32.logand addr (Int32.of_int (-linesize*4))
    
    (* index into cache line of an address (words) *)
    method idxaddr addr =
      Int32.logand (Int32.div addr (Int32.of_int linesize))
                   (Int32.of_int (linesize-1))
    
    (* try to allocate a cache line for an address.
     * raise Assoc_full on failure
     *)
    method try_allocate addr =
      let base = self#baseaddr addr in
      let set = assoc base in
      let rec seek = function
        [] -> raise Assoc_full
      | l::ls -> if idx.{l} = Int32.minus_one || idx.{l} = base
          then l else seek ls
      in
        let line = seek set in
        idx.{line} <- base;
        line
    
    (* return a closure plus a time at which that closure should
     * be executed, which will fill this cache with data from another place
     *)
    method fill line data =
      let cont () = self#writeline line data in
      (Int64.succ state#gettime, cont)
  end
  
