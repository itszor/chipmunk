class trace :
  object
    val mutable logfile : out_channel
    val mutable tracing : bool
    method put : string -> unit
    method start : string -> unit
    method stop : unit
    method active : bool
  end
