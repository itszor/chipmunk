class processor :
  object
    method bumptime : unit
    method fdiff : int -> bool
    method insntrace : Trace.trace
    method getblktrace : Trace.trace
    method getdlatch : Iformat.format -> Iformat.format
    method getflatch : int32 -> int32
    method getfreg : int -> float
    method getindirseg : int32
    method getnameseg : int32
    method getpc : int32
    method getprogseg : int32
    method getreg : int -> int32
    method gettime : int64
    method getx : int32
    method gety : int32
    method getz : int32
    method incrpc : unit
    method pipeflush : unit
    method putfreg : int -> float -> unit
    method putnameseg : int32 -> unit
    method putpc : int32 -> unit
    method putreg : int -> int32 -> unit
    method putx : int32 -> unit
    method puty : int32 -> unit
    method putz : int32 -> unit
    method rdiff : int -> bool
    method syncregs : unit
  end
