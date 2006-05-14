val writebinop : Ast.op2 -> Ast.ast -> Ast.ast -> string
val writecmp : Iformat.cond -> string
val writeunop : Ast.op1 -> Ast.ast -> string
val writebitfield : string -> int -> int -> string
val writereg : string -> int -> Id.loc -> Ast.usage -> string
val writevec : Ast.ast list -> string
val writeop : Ast.ast -> string
val writetreg : Ast.ldtype -> string
val writeloc : Id.loc -> string
val writectrl : Block.terminate -> string
val writedest : Block.blockref -> string
val writeinsns : Ast.ast list -> unit
val writeblk : Block.block -> unit
