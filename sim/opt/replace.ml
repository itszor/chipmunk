open Index
open I32op

let upgrade mach oldblk newcode =
  let proc = mach#getproc
  and mem = mach#getmem in
  let oldhdr = getindex mem proc oldblk in
  let newblk = mach#getserv#getindexer#alloc in
  Printf.fprintf stderr "Replacing block %ld with %ld\n" oldblk newblk;
  setindex mem proc newblk oldhdr;
  let newhdr = { offset = newcode; parent = oldblk; meta = 0l; count = 0l } in
  setindex mem proc oldblk newhdr
  
