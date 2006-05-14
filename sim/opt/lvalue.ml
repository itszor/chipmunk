(* Determine whether an AST node is an lvalue, and return the type of that
   lvalue (out of none, int register, single float register, double float 
   register, part of int register, memory location.
   Must be x in Move(x,_) of a Move node also.
*)

open Ast

type classification = LNone
                    | LReg of reg
                    | LFloat of reg
                    | LDouble of reg
                    | LIntPseudo of int
                    | LFloatPseudo of int
                    | LVector of Ast.ast list
                    | LMemory

let classify = function
    Register r -> LInt r
  | Unop(Cast_f Iformat.Single, Floatreg r) -> LFloat r
  | Unop(Cast_f Iformat.Double, Floatreg r) -> LDouble r
  | Binop(Bitfield, Pair _, Register r) -> failwith "No more part int"
  | Binop(SignedBitfield, Pair _, Register r) -> failwith "No more part int"
  | Unop(Ind _, _) -> LMemory
  | Vector(_,v) -> LVector(v)
  | Intpseudo(n,_) -> LIntPseudo n
  | Floatpseudo(n,_) -> LFloatPseudo n
  | _ -> LNone

let rewritereg ast classfc reg =
  match classfc with
    LInt _ -> reg
  | LFloat _ -> Unop(Cast_f Iformat.Single, reg)
  | LDouble _ -> Unop(Cast_f Iformat.Double, reg)
  | _ -> failwith "Cannot rewrite lvalue"
