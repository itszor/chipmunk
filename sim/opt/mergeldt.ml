open Ast
open Block

type values = {
  x: int32 option;
  y: int32 option;
  z: int32 option
}

exception MissingLdt

(* Search for ld/t instructions and merge with block terminator if required.
 * Does not deal with traps yet
 *)

let mergeldt blk =
  let Block(insns,term) = blk in
  let rec scanldt insns values ninsns =
    match insns with
      [] -> (values,ninsns)
    | l::ls ->
      begin match l with
        Move(Treg X, Constant v) ->
          scanldt ls {values with x=Some v} ninsns
      | Move(Treg Y, Constant v) ->
          scanldt ls {values with y=Some v} ninsns
      | Move(Treg Z, Constant v) ->
          scanldt ls {values with z=Some v} ninsns
      | Binop(Trap c, reg, Constant t) ->
          let zval = match values.z with
            Some n -> n
          | None -> raise MissingLdt
          in let newtrap =
            Binop(Trap c, reg,
                  Constant (Int32.logor zval (Int32.shift_left t 26)))
          in
          scanldt ls values (newtrap::ninsns)
      | _ ->
          scanldt ls values (l::ninsns)
      end
  and merge destblk valu =
    match (destblk,valu) with
      (PartialDirect n, Some otherbits) ->
        Direct(Int32.logor (Int32.shift_left n 26) otherbits)
    | (PartialDirect n, None) -> raise MissingLdt
    | x,_ -> x
  in
    let (v,ninsns) = scanldt (List.rev insns) {x=None; y=None; z=None} [] in
    let nterm = match term with
      CondBranch(prob, reg, trublk, falblk) ->
        CondBranch(prob, reg, merge trublk v.x, merge falblk v.y)
    | Call(callblk, retblk) ->
        Call(merge callblk v.x, merge retblk v.y)
    | Jump(jumpblk) ->
        Jump(merge jumpblk v.x)
    | Return ->
        Return
    in
      Block(ninsns,nterm)
