open State
open I32op

external load_elf : string -> int32 = "load_elf_c"

let idxbase    = 0x80000000l
and textbase   = 0x80000004l
and database   = 0x80000008l
and rodatabase = 0x8000000cl
and namebase   = 0x80000010l
and bssbase    = 0x80000014l
and lastidx    = 0x80000018l

let clearbss mach =
  let mem = mach#getmem in
  let base = ref (mem#readWord bssbase) in
  let numwords = mem#readWord !base in
  for x=0 to ((Int32.to_int numwords)-1)/4 do
    mem#writeWord !base Int32.zero;
    base := !base +! 4l;
  done

let blockname mach nameseg num =
  let proc = mach#getproc
  and mem = mach#getmem in
  let addr = proc#getindirseg +! ((num *! 16l) +! 8l) in
  let str = nameseg +! (mem#readWord addr) in
    Disassemble.vmgetstring mach str

let expand_i32 s =
  let l = ref [] in
  String.iter (fun q -> l := Int32.of_int (int_of_char q) :: !l) s;
  List.rev !l

let merge a b c d =
  Int32.logor
    (Int32.logor
      (Int32.logor a (Int32.shift_left b 8))
      (Int32.shift_left c 16))
    (Int32.shift_left d 24)

let int32array_of_string x =
  let length = ((String.length x)/4)+1 in
  let array = Array.create length 0l in
  let rec fill idx str =
    match str with
      a::b::c::d::rest -> array.(idx) <- merge a b c d; fill (idx+1) rest
    | a::b::c::rest -> array.(idx) <- merge a b c 0l
    | a::b::rest -> array.(idx) <- merge a b 0l 0l
    | a::rest -> array.(idx) <- merge a 0l 0l 0l
    | [] -> ()
  in
    fill 0 (expand_i32 x);
    array

(* Convert meta field in index to Tag_canonical_name metadata *)
let makecanonicalnames mach nameseg =
  let mem = mach#getmem in
  let indices = Int32.to_int (mem#readWord lastidx) in
  for i=0 to indices-1 do
    let i32 = Int32.of_int i in
    let name = blockname mach nameseg i32 in
    Tag.put_metadata mach i32 Tag.Tag_canonical_name
                          (int32array_of_string name)
  done
