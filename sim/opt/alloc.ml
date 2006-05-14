open Common
open Memory
open I32op

(* let's make a class to do memory allocation inside emulator-space *)

exception AllocationError

class zone thebase thelimit =
  object (self)
  
  val base = thebase
  val limit = thelimit
  val mutable ptr = thebase

  method allocate size =
    let newblk = ptr in
    ptr <- ptr +! size;
    if (Int32.logand size 3l) <> 0l then
      failwith "Allocation size must be a multiple of 4";
    if ptr>limit then raise AllocationError;
    newblk
  
  method clear =
    ptr <- base

end
