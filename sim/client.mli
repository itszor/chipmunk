val get_my_addr : unit -> Unix.inet_addr
val sendblk :
  < readWord : int32 -> int32; .. > ->
  < getindirseg : int32; getprogseg : int32; .. > ->
  out_channel -> int32 -> unit
val sendref :
  < readWord : int32 -> int32; .. > ->
  < getindirseg : int32; .. > -> out_channel -> int32 -> unit
val injectref :
  < writeWord : int32 -> 'a -> 'b; .. > ->
  < getindirseg : int32; .. > -> 'a list -> int32 -> unit
val injectblk :
  < readWord : int32 -> int32; writeWord : int32 -> 'a -> 'b; .. > ->
  < getindirseg : int32; getprogseg : int32; .. > -> 'a list -> int32 -> unit
module Funs : sig val add : Channel.wrapper -> Channel.wrapper end
val clientchannel : unit -> Channel.channel
val populateclient :
  < makelocal : string -> (Channel.wrapper -> Channel.wrapper) -> 'a; .. > ->
  'a
