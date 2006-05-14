(* Fake system calls *)
(* Marshal register arguments to/from DOP space *)

open State

let prefix =
  try
    UnixLabels.getenv "DOP_ROOT"
  with Not_found -> ""

let string_of_char c = String.make 1 c

let getstring mem addr =
  let rec build ptr =
    let c = mem#readByte ptr in
    if c = Int32.zero then "" else
      string_of_char (char_of_int (Int32.to_int c)) ^ build (Int32.succ ptr)
  in
    build addr

exception BadMaths

let convflags f =
  let flags = ref [] in
  let testmask flags msk =
    Int32.logand flags (Int32.of_int msk) <> Int32.zero
  in 
  begin match Int32.to_int (Int32.logand f (Int32.of_int 3)) with
    0 -> flags := Unix.O_RDONLY :: !flags
  | 1 -> flags := Unix.O_WRONLY :: !flags
  | 2 -> flags := Unix.O_RDWR :: !flags
  | 3 -> ()
  | _ -> raise BadMaths
  end;
  if testmask f 0x4000 then flags := Unix.O_NONBLOCK :: !flags;
  if testmask f 0x0008 then flags := Unix.O_APPEND :: !flags;
  if testmask f 0x0200 then flags := Unix.O_CREAT :: !flags;
  if testmask f 0x0400 then flags := Unix.O_TRUNC :: !flags;
  if testmask f 0x0800 then flags := Unix.O_EXCL :: !flags;
  if testmask f 0x8000 then flags := Unix.O_NOCTTY :: !flags;
  if testmask f 0x2000 then flags := Unix.O_SYNC :: !flags;
  !flags

let sys_open mach =
  let processor = mach#getproc in
  let pathname = getstring mach#getmem (processor#getreg 0)
  and flags = processor#getreg 1
  and mode = processor#getreg 2
  in
  try
    let filedes = UnixLabels.openfile
      (prefix ^ "/" ^ pathname)
      ~mode:(convflags flags)
      ~perm:(Int32.to_int mode)
    in
    let number = mach#getserv#newdescriptor filedes in
    processor#putreg 0 (Int32.of_int number)
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

let sys_close mach =
  let processor = mach#getproc in
  let descr = Int32.to_int (processor#getreg 0) in
  let filedes = mach#getserv#getdescriptor descr in
  try
    UnixLabels.close filedes;
    mach#getserv#killdescriptor descr;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

let sys_read mach =
  let processor = mach#getproc in
  let fdnum = Int32.to_int (processor#getreg 0)
  and buf = ref (processor#getreg 1)
  and count = Int32.to_int (processor#getreg 2) in
  let descr = mach#getserv#getdescriptor fdnum
  and stringbuf = String.make count '\000' in
  if count=0 then
    processor#putreg 0 Int32.zero
  else begin
    let actuallyread =
      UnixLabels.read descr ~buf:stringbuf ~pos:0 ~len:count
    in
    for i=0 to actuallyread-1 do
      let byte = (Int32.of_int (int_of_char stringbuf.[i])) in
      mach#getmem#writeByte !buf byte;
      buf := Int32.succ !buf
    done;
    processor#putreg 0 (Int32.of_int actuallyread)
  end

let sys_write mach =
  let processor = mach#getproc
  and mem = mach#getmem in
  let fdnum = Int32.to_int (processor#getreg 0)
  and buf = ref (processor#getreg 1)
  and count = Int32.to_int (processor#getreg 2) in
  let descr = mach#getserv#getdescriptor fdnum
  and stringbuf = String.make count '\000'
  in
  for i=0 to count-1 do
    stringbuf.[i] <- (char_of_int (Int32.to_int (mem#readByte !buf)));
    buf := Int32.succ !buf
  done;
  try
    let actuallywrote =
      UnixLabels.write descr ~buf:stringbuf ~pos:0 ~len:count
    in
      processor#putreg 0 (Int32.of_int actuallywrote)
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one
  
let sys_time mach =
  let ptr = mach#getproc#getreg 0 in
  let time = Int32.of_float (UnixLabels.time ()) in
  if ptr <> Int32.zero then
    mach#getmem#writeWord ptr time;
  mach#getproc#putreg 0 time

(*
From newlib/include/sys/stat.h:

struct  stat
{
                             size offset
  dev_t         st_dev;      2    0
  ino_t         st_ino;      2    2
  mode_t        st_mode;     4    4
  nlink_t       st_nlink;    2    8
  uid_t         st_uid;      2    10
  gid_t         st_gid;      2    12
  dev_t         st_rdev;     2    14
  off_t         st_size;     4    16
  time_t        st_atime;    4    20
  long          st_spare1;   4    24
  time_t        st_mtime;    4    28
  long          st_spare2;   4    32
  time_t        st_ctime;    4    36
  long          st_spare3;   4    40
  long          st_blksize;  4    44
  long          st_blocks;   4    48
  long  st_spare4[2];        8    52
};

This is subject, of course, to extreme breakage.
*)

let fillstatbuf mem buf stats =
  let ww base idx word =
    mem#writeWord (Int32.add base (Int32.of_int idx)) word
  and wh base idx half =
    mem#writeHalfword (Int32.add base (Int32.of_int idx)) half
  and transkind = function
    Unix.S_REG -> Int32.of_int 0o100000
  | Unix.S_DIR -> Int32.of_int 0o040000
  | Unix.S_CHR -> Int32.of_int 0o020000
  | Unix.S_BLK -> Int32.of_int 0o060000
  | Unix.S_LNK -> Int32.of_int 0o120000
  | Unix.S_FIFO -> Int32.of_int 0o010000
  | Unix.S_SOCK -> Int32.of_int 0o140000
  in
  wh buf 0 (Int32.of_int stats.Unix.st_dev);
  wh buf 2 (Int32.of_int stats.Unix.st_ino);
  ww buf 4 (Int32.logor (transkind stats.Unix.st_kind)
                        (Int32.of_int stats.Unix.st_perm));
  wh buf 8 (Int32.of_int stats.Unix.st_nlink);
  wh buf 10 (Int32.of_int stats.Unix.st_uid);
  wh buf 12 (Int32.of_int stats.Unix.st_gid);
  wh buf 14 (Int32.of_int stats.Unix.st_rdev);
  ww buf 16 (Int32.of_int stats.Unix.st_size);
  ww buf 20 (Int32.of_float stats.Unix.st_atime);
  ww buf 28 (Int32.of_float stats.Unix.st_mtime);
  ww buf 36 (Int32.of_float stats.Unix.st_ctime)

let sys_fstat mach =
  let processor = mach#getproc in
  let fdnum = Int32.to_int (processor#getreg 0)
  and buf = (processor#getreg 1) in
  let descr = mach#getserv#getdescriptor fdnum in
  try
    let stats = UnixLabels.fstat descr in
    fillstatbuf mach#getmem buf stats;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

let sys_lstat mach =
  let processor = mach#getproc in
  let filename = getstring mach#getmem (processor#getreg 0)
  and bufptr = (processor#getreg 1) in
  try
    let stats = UnixLabels.lstat filename in
    fillstatbuf mach#getmem bufptr stats;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one
    
let sys_stat mach =
  let processor = mach#getproc
  and mem = mach#getmem in
  let filename = getstring mem (processor#getreg 0)
  and bufptr = (processor#getreg 1) in
  try
    let stats = UnixLabels.stat filename in
    fillstatbuf mem bufptr stats;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

let sys_exit mach =
  let exitcode = mach#getproc#getreg 0 in
  mach#getserv#getprofiler#dumprecur;
  exit (Int32.to_int exitcode)

let sys_dup mach =
  let processor = mach#getproc in
  let fdnum = processor#getreg 0 in
  let descr = mach#getserv#getdescriptor (Int32.to_int fdnum) in
  let dupl = UnixLabels.dup descr in
  let number = mach#getserv#newdescriptor dupl in
  processor#putreg 0 (Int32.of_int number)

(* Not too sure about the semantics here *)
let sys_dup2 mach =
  let processor = mach#getproc in
  let oldfdnum = processor#getreg 0
  and newfdnum = Int32.to_int (processor#getreg 1) in
  let olddescr = mach#getserv#getdescriptor (Int32.to_int oldfdnum) in
  if mach#getserv#querydescriptor newfdnum then begin
    let newdescr = mach#getserv#getdescriptor newfdnum in
    UnixLabels.dup2 ~src:olddescr ~dst:newdescr;
    processor#putreg 0 (Int32.of_int newfdnum);
    end
  else begin
    let newdescr = UnixLabels.dup olddescr in
    mach#getserv#insertdescriptor newfdnum newdescr;
    processor#putreg 0 (Int32.of_int newfdnum)
  end

let sys_chown mach =
  let processor = mach#getproc in
  let name = getstring mach#getmem (processor#getreg 0)
  and uid = Int32.to_int (processor#getreg 1)
  and gid = Int32.to_int (processor#getreg 2) in
  try
    UnixLabels.chown name ~uid:uid ~gid:gid;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one
  
let sys_chmod mach =
  let processor = mach#getproc in
  let name = getstring mach#getmem (processor#getreg 0)
  and mode = Int32.to_int (processor#getreg 1) in
  try
    UnixLabels.chmod name mode;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

exception BadSeekCommand

let sys_lseek mach =
  let processor = mach#getproc in
  let fdnum = Int32.to_int (processor#getreg 0)
  and pos = Int32.to_int (processor#getreg 1)
  and cmd = match Int32.to_int (processor#getreg 2) with
    0 -> Unix.SEEK_SET
  | 1 -> Unix.SEEK_CUR
  | 2 -> Unix.SEEK_END
  | _ -> raise BadSeekCommand
  in let descr = mach#getserv#getdescriptor fdnum in
  let res = UnixLabels.lseek descr pos ~mode:cmd in
  processor#putreg 0 (Int32.of_int res)

let sys_unlink mach =
  let processor = mach#getproc in
  let filename = getstring mach#getmem (processor#getreg 0) in
  try
    UnixLabels.unlink filename;
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

let sys_utime mach =
  let processor = mach#getproc in
  let mem = mach#getmem in
  let filename = getstring mach#getmem (processor#getreg 0)
  and bufptr = (processor#getreg 1) in
  try
    let atime = mem#readWord bufptr
    and mtime = mem#readWord (Int32.add bufptr (Int32.of_int 4)) in
    UnixLabels.utimes filename ~access:(Int32.to_float atime)
      ~modif:(Int32.to_float mtime);
    processor#putreg 0 Int32.zero
  with Unix.Unix_error _ ->
    processor#putreg 0 Int32.minus_one

exception BadSyscall of int

let dispatch call machine =
  let syscall =
    match call with
      0 -> sys_exit
    | 1 -> sys_read
    | 2 -> sys_write
    | 3 -> sys_open
    | 4 -> sys_close
    | 5 -> sys_fstat
    | 6 -> sys_time
    | 7 -> sys_dup
    | 8 -> sys_dup2
    | 9 -> sys_chown
    | 10 -> sys_chmod
    | 11 -> sys_lseek
    | 12 -> sys_lstat
    | 13 -> sys_stat
    | 14 -> sys_unlink
    | 15 -> sys_utime
    | _ -> raise (BadSyscall call)
  in
    syscall machine
  
(*let dispatch =
[| sys_exit;   (* 0 *)
   sys_read;   (* 1 *)
   sys_write;  (* 2 *)
   sys_open;   (* 3 *)
   sys_close;  (* 4 *)
   sys_fstat;  (* 5 *)
   sys_time;   (* 6 *)
   sys_dup;    (* 7 *)
   sys_dup2;   (* 8 *)
   sys_chown;  (* 9 *)
   sys_chmod;  (* 10 *)
   sys_lseek;  (* 11 *)
   sys_lstat;  (* 12 *)
   sys_stat;   (* 13 *)
   sys_unlink; (* 14 *)
   sys_utime   (* 15 *)
|]
*)
