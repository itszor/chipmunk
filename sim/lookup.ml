open Common
open State

(* Look up indirect code reference *)
let lookup mem proc x =
 (* state.blocktrace#put (Printf.sprintf "%ld\n" x);*)
  let offset = mem#readWord (Int32.add proc#getindirseg x)
  in
(*    Printf.printf "Looked up %lx for %lx\n" offset x;
    Pervasives.flush stdout;*)
    Int32.add proc#getprogseg offset
