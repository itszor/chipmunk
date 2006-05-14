(* Flatten the control part of a block into its insn part, ready for
 * instruction selection
 *)

open Block
open Asmsupport

exception BadTransfer

let mergetransfer vertices ast tfer =
  let blockref = function
      Direct d -> Ast.Constant(d)
    | Indirect i -> i
    | Local t ->
        begin match t.writeidx with
          None -> failwith ("No index to write block with dfnum " ^
                            (string_of_int t.dfnum))
        | Some dest -> Ast.Constant(Int32.mul dest 16l)
        end
    | _ -> raise BadTransfer
  in let endpart = match tfer with
    CondBranch(_,rc,t,f) -> Ast.Triop(Ast.Branch,
      rc, blockref t, blockref f)
  | Call(c,r) -> Ast.Binop(Ast.Call, blockref c, blockref r)
  | Jump(j) -> Ast.Unop(Ast.Jump, blockref j)
  | Return -> Ast.Zop(Ast.Return)
  in
    List.rev (endpart :: ast)
