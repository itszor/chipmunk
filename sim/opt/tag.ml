open I32op

type tag = Tag_in_probation
         | Tag_never_optimise
         | Tag_trap_converted
         | Tag_jump_folded
         | Tag_specialised_data_profile
         | Tag_canonical_name
         | Tag_timing_instrumented

let int_of_tag = function
    Tag_in_probation -> 0
  | Tag_never_optimise -> 1
  | Tag_trap_converted -> 2
  | Tag_jump_folded -> 3
  | Tag_specialised_data_profile -> 4
  | Tag_canonical_name -> 5
  | Tag_timing_instrumented -> 6

exception BadMetadata of int

let tag_of_int = function
    0 -> Tag_in_probation
  | 1 -> Tag_never_optimise
  | 2 -> Tag_trap_converted
  | 3 -> Tag_jump_folded
  | 4 -> Tag_specialised_data_profile
  | 5 -> Tag_canonical_name
  | 6 -> Tag_timing_instrumented
  | n -> raise (BadMetadata n)

let put_metadata machine blkno tag data =
  let mem = machine#getmem
  and proc = machine#getproc in
  let datalength = Array.length data
  and idx = Index.getindex mem proc blkno in
  begin match datalength with
    0 ->
      let meta = Int32.logor 1l (Int32.shift_left
        (Int32.of_int (int_of_tag tag)) 24)
      in
        idx.Index.meta <- meta
  | n ->
      let allocator = machine#getserv#getallocator in
      let newblk = allocator#allocate !!((n+1)*4) in
      idx.Index.meta <- newblk;
      let tagsize = Int32.logor (Int32.shift_left
        !!(int_of_tag tag) 24) (!!n) in
      mem#writeWord newblk tagsize;
      for i=0 to n-1 do
        mem#writeWord (newblk +! !!((i+1)*4)) data.(i)
      done
  end;
  Index.setindex mem proc blkno idx

let get_metadata machine blkno =
  let mem = machine#getmem
  and proc = machine#getproc in
  let idx = Index.getindex mem proc blkno in
  let meta = idx.Index.meta in
  if (Int32.logand meta 1l) = 1l then
    (tag_of_int (Int32.to_int (Int32.shift_right_logical meta 24)), [||])
  else begin
    let hdr = mem#readWord meta in
    let tag = Int32.to_int (Int32.shift_right_logical hdr 24)
    and size = Int32.to_int (Int32.logand hdr 0x00ffffffl) in
    let data = Array.create size 0l in
    for i=0 to size-1 do
      data.(i) <- mem#readWord (meta +! (Int32.of_int ((i+1)*4)))
    done;
    (tag_of_int tag, data)
  end

let string_of_char c =
  let s = String.create 1 in
  s.[0] <- c;
  s

let string_of_int32array arr =
  let rec split x =
    match Int32.logand x 0xffl with
      0l -> ""
    | n -> (string_of_char (char_of_int (Int32.to_int n))) ^
           (split (Int32.shift_right_logical x 8))
  in let buf =
    Array.fold_left (fun a b -> Buffer.add_string a (split b); a)
                    (Buffer.create 8)
                    arr
  in
    Buffer.contents buf

let blockname mach blkno =
  let tag,data = get_metadata mach blkno in
  match tag with
    Tag_canonical_name -> string_of_int32array data
  | Tag_trap_converted -> (Int32.to_string blkno) ^ "_(trap_converted)"
  | Tag_jump_folded -> (Int32.to_string blkno) ^ "_(jump_folded)"
  | _ -> (Int32.to_string blkno) ^ "_(opt)"

