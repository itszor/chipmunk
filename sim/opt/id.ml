(* Register ID (number and "location" of definition)
   "location" because it only has to be different from other locations,
   ie monotonically increasing.
*)

type id = int * loc

and loc = Unset | Suf of int

and regtype = IntType | PtrType | FloatType

type reg_or_pseudo = PhysReg of id * regtype
                   | PseudoReg of int * regtype

let locstr = function
    Unset -> ""
  | Suf i -> "[" ^ string_of_int i ^ "]"

let string_of_regtype = function
    IntType -> "r"
  | PtrType -> "p"
  | FloatType -> "f"

let string_of_rop = function
    PhysReg((num,loc),typ) -> Printf.sprintf "%s%d%s"
                                (string_of_regtype typ) num (locstr loc)
  | PseudoReg(num,typ) -> Printf.sprintf "i%s%d" (string_of_regtype typ) num
