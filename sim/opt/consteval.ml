(* Constant expression evaluation/folding *)

open Ast

let binop op a b =
  match op with
    Lsl -> Constant(Int32.shift_left a (Int32.to_int b))
  | Lsr -> Constant(Int32.shift_right_logical a (Int32.to_int b))
  | Asr -> Constant(Int32.shift_right a (Int32.to_int b))
  | Ror ->
      let amt = (Int32.to_int b) in
      Constant(Int32.logor (Int32.shift_right_logical a amt)
                           (Int32.shift_left a (32-amt)))
  | And -> Constant(Int32.logand a b)
  | Ior -> Constant(Int32.logor a b)
  | Eor -> Constant(Int32.logxor a b)
  | Add -> Constant(Int32.add a b)
  | Sub -> Constant(Int32.sub a b)
  | Mul -> Constant(Int32.mul a b)
  | Div -> Constant(Int32.div a b)
  | Udiv -> Constant(Common.uDiv a b)
  | Mod -> Constant(Int32.rem a b)
  | Umod -> Constant(Common.uRem a b)
  | _ -> Binop(op, Constant a, Constant b)

let unop op a =
  match op with
    Not -> Constant(Int32.lognot a)
  | _ -> Unop(op, Constant a)

let dosplice lhs rhs splpt withshift =
  let mask = Int32.shift_left Int32.minus_one splpt in
  let imask = Int32.lognot mask in
  let rhs' = if withshift then Int32.shift_left rhs splpt else rhs in
  Int32.logor (Int32.logand lhs imask) (Int32.logand rhs' mask)

(* Could conceivably be a non-bitfield operation with form
 * (op (#l #h) #y), but it's not very likely really seeing as it would be
 * a "syntax error".
 *)
let triop op l h y =
  match op with
    Bitfield -> Constant(Common.extractbits y (Int32.to_int l)
                         (Int32.to_int h) false)
  | SignedBitfield -> Constant(Common.extractbits y (Int32.to_int l) 
                               (Int32.to_int h) true)
  | SpliceShift -> Constant(dosplice l h (Int32.to_int y) true)
  | Splice -> Constant(dosplice l h (Int32.to_int y) false)
  | _ -> Triop(op, Constant l, Constant h, Constant y)

let rec eval ast =
  match ast with
    Triop(op, a, b, c) ->
      let a' = eval a
      and b' = eval b
      and c' = eval c in
      begin match a', b', c' with
        Constant(l), Constant(h), Constant(y) -> triop op l h y
      | _ -> Triop(op, a', b', c')
      end
  | Binop(op, a, b) ->
      let a' = eval a
      and b' = eval b in
      begin match a', b' with
        Constant(x), Constant(y) -> binop op x y
      | _ -> Binop(op, a', b')
      end
  | Unop(op, a) ->
      let a' = eval a in
      begin match a' with
        Constant(x) -> unop op x
      | _ -> Unop(op, a')
      end
  | Move(a, b) ->
      let a' = eval a
      and b' = eval b in
      Move(a', b')
  | x -> x
  
let evalvertices vertices =
  for i=0 to (DynArray.length vertices)-1 do
    let tag = DynArray.get vertices i in
    let block = tag.Block.block in
    let Block.Block(astlist, term) = block in
    let astlist' =
      List.fold_right
        (fun ast acc -> eval ast :: acc)
        astlist
        []
    in
      tag.Block.block <- Block.Block(astlist', term)
  done
