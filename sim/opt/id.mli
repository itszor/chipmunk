type id = int * loc

and loc = Unset | Suf of int

and regtype = IntType | PtrType | FloatType

type reg_or_pseudo = PhysReg of id * regtype
                   | PseudoReg of int * regtype

val string_of_rop : reg_or_pseudo -> string
