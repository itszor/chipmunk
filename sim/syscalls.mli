val prefix : string
val string_of_char : char -> string
val getstring : < readByte : int32 -> int32; .. > -> int32 -> string
exception BadMaths
val convflags : int32 -> Unix.open_flag list
val sys_open :
  < getmem : < readByte : int32 -> int32; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    getserv : < newdescriptor : UnixLabels.file_descr -> int; .. >; .. > ->
  'a
val sys_close :
  < getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr;
                killdescriptor : int -> 'b; .. >;
    .. > ->
  'a
val sys_read :
  < getmem : < writeByte : int32 -> int32 -> 'a; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'b; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr; .. >; .. > ->
  'b
val sys_write :
  < getmem : < readByte : int32 -> int32; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr; .. >; .. > ->
  'a
val sys_time :
  < getmem : < writeWord : int32 -> int32 -> unit; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    .. > ->
  'a
val fillstatbuf :
  < writeHalfword : int32 -> int32 -> 'a; writeWord : int32 -> int32 -> 'b;
    .. > ->
  int32 -> Unix.stats -> 'b
val sys_fstat :
  < getmem : < writeHalfword : int32 -> int32 -> 'a;
               writeWord : int32 -> int32 -> 'b; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'c; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr; .. >; .. > ->
  'c
val sys_lstat :
  < getmem : < readByte : int32 -> int32;
               writeHalfword : int32 -> int32 -> 'a;
               writeWord : int32 -> int32 -> 'b; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'c; .. >;
    .. > ->
  'c
val sys_stat :
  < getmem : < readByte : int32 -> int32;
               writeHalfword : int32 -> int32 -> 'a;
               writeWord : int32 -> int32 -> 'b; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'c; .. >;
    .. > ->
  'c
val sys_exit :
  < getproc : < getreg : int -> int32; .. >;
    getserv : < getprofiler : < dumprecur : 'a; .. >; .. >; .. > ->
  'b
val sys_dup :
  < getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr;
                newdescriptor : UnixLabels.file_descr -> int; .. >;
    .. > ->
  'a
val sys_dup2 :
  < getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr;
                insertdescriptor : int -> UnixLabels.file_descr -> 'b;
                querydescriptor : int -> bool; .. >;
    .. > ->
  'a
val sys_chown :
  < getmem : < readByte : int32 -> int32; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    .. > ->
  'a
val sys_chmod :
  < getmem : < readByte : int32 -> int32; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    .. > ->
  'a
exception BadSeekCommand
val sys_lseek :
  < getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr; .. >; .. > ->
  'a
val sys_unlink :
  < getmem : < readByte : int32 -> int32; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    .. > ->
  'a
val sys_utime :
  < getmem : < readByte : int32 -> int32; readWord : int32 -> int32; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'a; .. >;
    .. > ->
  'a
exception BadSyscall of int
val dispatch :
  int ->
  < getmem : < readByte : int32 -> int32; readWord : int32 -> int32;
               writeByte : int32 -> int32 -> 'a;
               writeHalfword : int32 -> int32 -> 'b;
               writeWord : int32 -> int32 -> unit; .. >;
    getproc : < getreg : int -> int32; putreg : int -> int32 -> 'c; .. >;
    getserv : < getdescriptor : int -> UnixLabels.file_descr;
                getprofiler : < dumprecur : 'd; .. >;
                insertdescriptor : int -> UnixLabels.file_descr -> 'e;
                killdescriptor : int -> 'f;
                newdescriptor : UnixLabels.file_descr -> int;
                querydescriptor : int -> bool; .. >;
    .. > ->
  'c
