open Common
open Fetch
open Decode
open Exec
open State

let (++) a b = b a

(* Move pipeline along *)
let shift mach =
  let proc = mach#getproc
  and mem = mach#getmem in
  fetch mem proc ++ proc#getflatch ++ decode ++ proc#getdlatch ++ exec mach

let nop = Iformat.NoopF

(* Pass old decoded instruction back to exec (stalled exec) *)
(*let execstall state =
  let olddecode = state.dlatch nop in
  ignore (state.dlatch olddecode); exec state olddecode*)

let currentinst proc =
  let cinst = proc#getdlatch nop in
  ignore (proc#getdlatch cinst); cinst

let currentrawinst mem proc =
  let getfrom = Int32.sub proc#getpc 8l in
  mem#readWord getfrom

(* Stalled on fetch, keep trying... but does this lose an instruction? *)
(*let fetchstall state =
  ignore (fetch state ++ state.flatch) *)

let trace state =
  let thisinst = Disassemble.diss state (currentinst state#getproc)
  and rawinst = currentrawinst state#getmem state#getproc in
  Printf.sprintf "Time %Lx pc=%lx %.8lx: %-25s\t " state#getproc#gettime 
                state#getproc#getpc rawinst thisinst

let alteredregs mach proc =
  let buf = Buffer.create 50 in
  for i=0 to 63 do
    if proc#rdiff i then
      if i=63 then
        let block = Int32.shift_right_logical (proc#getreg 63) 4 in
        Buffer.add_string buf
          (Printf.sprintf "r63 <- %s; " (Tag.blockname mach block))
      else
        Buffer.add_string buf
          (Printf.sprintf "r%d <- %.8lx; " i (proc#getreg i))
  done;
  for i=0 to 63 do
    if proc#fdiff i then
      Buffer.add_string buf
        (Printf.sprintf "f%d <- %g; " i (proc#getfreg i))
  done;
  Buffer.add_string buf "\n";
  Buffer.contents buf

let run_with_trace mach =
  let proc = mach#getproc in
  while true do
    proc#syncregs;
    proc#insntrace#put (trace mach);
    (* if state.stalled then execstall state else *)
    shift mach;
    proc#insntrace#put (alteredregs mach proc);
    (*if state.trace then begin pregs state; end;*)
    proc#bumptime
  done

let run mach =
  let proc = mach#getproc in
  while true do
    shift mach; proc#bumptime
  done
