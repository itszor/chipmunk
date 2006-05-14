type values = { x : int32 option; y : int32 option; z : int32 option; }
exception MissingLdt
val mergeldt : Block.block -> Block.block
