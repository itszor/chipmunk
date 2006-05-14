open Common

(* Execute a given block atomically *)
let executeblock mach blk =
  let fetched = Fetch.fetchblock mach blk in
  let decoded = List.map Decode.decode fetched in
  List.iter (Exec.exec mach) decoded

let run mach =
  let proc = mach#getproc in
  while true do
    executeblock mach (proc#getreg 63)
  done
