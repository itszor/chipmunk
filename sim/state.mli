class disassembler_helper :
  object
    method setx : int32 -> unit
    method sety : int32 -> unit
    method setz : int32 -> unit
    method getx : int32
    method gety : int32
    method getz : int32
  end

class services :
  object
    method getallocator : Alloc.zone
    method getindexer : Index.indexer
    method getprofiler : Profiler.profiler
    method getdescriptor : int -> Unix.file_descr
    method getdishelp : disassembler_helper
    method insertdescriptor : int -> Unix.file_descr -> unit
    method killdescriptor : int -> unit
    method newdescriptor : Unix.file_descr -> int
    method querydescriptor : int -> bool
    method getoptlimit : int option
    method setoptlimit : int option -> unit
    method optimiser : bool -> unit
    method testoptok : bool
    method bumpopt : unit
  end
 
class machine :
  object
    method getmem : Memory.memory
    method getproc : Processor.processor
    method getserv : services
  end
