module ParseArgs :
  sig
    val anonarglist : string list ref
    val progname : string ref
    val anonarg : string -> unit
    val trace : bool ref
    val dumptrace : bool ref
    val dumpread : bool ref
    val dumpwrite : bool ref
    val argvbase : int32
    val argvstrings : int32 ref
    val injectargs : State.machine -> string list -> unit
  end
module RunMe : sig  end
