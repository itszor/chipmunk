open Common
open Iformat

(*
type dtype = DRaw of (int32 -> format)
           | DCached of (int32 -> State.state -> unit)

let decodecache : (int32,format) Hashtbl.t = Hashtbl.create 1031 
*)

let decode inst =
  decodeArray.(Int32.to_int (Int32.shift_right_logical inst 26)) inst
