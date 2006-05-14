open State
open Common
open Memory
open I32op

let fetch mem proc =
  mem#readWord proc#getpc

(* Fetches a block. The "first" instruction is at the head of the list. *)
let fetchblock mach blk =
  let mem = mach#getmem in
  let rec uptoterm addr =
    let raw = mem#readWord addr in
    let opcode = Int32.to_int (Int32.shift_right_logical raw 26) in
    if opcode<53 || opcode>56 then
      raw :: uptoterm (addr +! 4l)
    else
      [raw]
  in
    let startaddr = Lookup.lookup mem mach#getproc blk in
    uptoterm startaddr
