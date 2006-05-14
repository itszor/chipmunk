val dump : State.machine -> out_channel -> Block.tag DynArray.t
             -> ?extra:(out_channel -> unit)
             -> [> `VERTEX | `ASSEMBLY | `PRED | `SUCC | `DOMFRONT |
                   `LIVENESS] list
             -> unit
