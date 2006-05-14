open Common
open Bigarray

class disassembler_helper =
  object (self)
    val mutable xval = 0l
    val mutable yval = 0l
    val mutable zval = 0l
    
    method setx a = xval <- a
    method sety a = yval <- a
    method setz a = zval <- a
    
    method getx = xval
    method gety = yval
    method getz = zval
  end

class services =
  object (self)
    val profiler = new Profiler.profiler
    val allocator = new Alloc.zone 0x90000000l 0x9fffffffl
    val indexer = new Index.indexer 0l
    val filedestab = Hashtbl.create 17
    val mutable newfiledes = 10
    val disassembler_helper = new disassembler_helper
    val mutable blocklimit = None
    val mutable blockcount = 0
    val mutable enable_opt = false

    method newdescriptor (mapto : Unix.file_descr) =
      let number = newfiledes in
      Hashtbl.add filedestab number mapto;
      newfiledes <- newfiledes+1;
      number
    
    method getdescriptor d =
      (Hashtbl.find filedestab d : Unix.file_descr)
    
    method killdescriptor (d:int) =
      (Hashtbl.remove filedestab d : unit)
    
    method querydescriptor d =
      (Hashtbl.mem filedestab d : bool)
    
    method insertdescriptor (d : int) (mapto : Unix.file_descr) =
      Hashtbl.add filedestab d mapto
    
    method getprofiler = profiler

    method getallocator = allocator
    
    method getindexer = indexer
    
    method getdishelp = disassembler_helper
    
    method setoptlimit n =
      blocklimit <- n

    method optimiser active =
      enable_opt <- active
    
    method getoptlimit = blocklimit
    
    method testoptok = enable_opt
    
    method bumpopt =
      blockcount <- blockcount + 1;
      match blocklimit with
        None -> ()
      | Some limit ->
          if blockcount >= limit then
            enable_opt <- false
      
  end  

class machine =
  object (self)
    val processor = new Processor.processor
    val memory = new Memory.memory
    val services = new services
  
    method getproc = processor
    method getmem = memory
    method getserv = services
  end

(*type state = {
  registers: int32 array;
  oldregs: int32 array;
  floatregs: float array;
  oldfloats: float array;
  mutable x_reg: int32;
  mutable y_reg: int32;
  mutable z_reg: int32;
  mutable c_reg: int;
  flatch: (int32 -> int32);
  dlatch: (Iformat.format -> Iformat.format);
  mutable xfertype: Iformat.xfer;
  memory: chunk array;
  mutable program_counter: int32;
  mutable indirection_seg: int32;
  mutable program_seg: int32;
  mutable name_seg: int32;
  mutable time: int64;
  mutable stalled: bool;
  mutable trace: bool;
  mutable blocktrace: Trace.trace;
  mutable readtrace: Trace.trace;
  mutable writetrace: Trace.trace;
  mutable profiler: Profiler.profiler;
  filedestab: (int,Unix.file_descr) Hashtbl.t;
  mutable newfiledes: int
}*)

(* An initial processor state *)
(*let processor = {
  registers = Array.make 64 Int32.zero;
  oldregs = Array.make 64 Int32.zero;
  floatregs = Array.make 64 0.0;
  oldfloats = Array.make 64 0.0;
  x_reg = Int32.zero;
  y_reg = Int32.zero;
  z_reg = Int32.zero;
  c_reg = 0;
  flatch = latch (ref (Int32.shift_left 60l 26));
  dlatch = latch (ref (Iformat.NoopF));
  xfertype = Iformat.Xstop;
  memory = Array.make 4096 ChunkEmpty;
  program_counter = Int32.zero;
  indirection_seg = Int32.zero;
  program_seg = 0x10000000l;
  name_seg = Int32.zero;
  time = Int64.zero;
  stalled = false;
  trace = false;
  blocktrace = new Trace.trace;
  readtrace = new Trace.trace;
  writetrace = new Trace.trace;
  profiler = new Profiler.profiler;
  filedestab = Hashtbl.create 17;
  newfiledes = 10
}
;;

processor.memory.(3840) <- ChunkIO(Serial.serialread, Serial.serialwrite)
;;
*)
