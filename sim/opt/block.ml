(* Basic blocks to keep code in *)

open Ast

(* Maybe we should use some sort of 'confidence' measure for the weight
 * (float)
 *)
type terminate = CondBranch of float * ast * blockref * blockref
               | Call of blockref * blockref
               | Jump of blockref
               | Return

and blockref = Indirect of ast
             | Direct of int32
             | PartialDirect of int32
             | Local of tag
         (*  | Self of int32 *)

and block = Block of ast list * terminate

and tag = {
  mutable domfront : tag list;
  mutable parent : tag option;
  mutable predecessors : tag list;
  mutable successors : tag list;
  mutable semi : tag option;
  mutable idom : tag option;
  mutable idomchild : tag list;
  mutable ancestor : tag option;
  mutable samedom : tag option;
  mutable bucket : tag list;
  mutable dfnum : int;
  mutable block : block;
  mutable selected : Select.selectedast list;
  mutable refno : int32;
  mutable writeidx : int32 option;
  mutable live_in : Sets.RegOrPseudoSet.t;
  mutable live_out : Sets.RegOrPseudoSet.t
}

and colour = White | Grey | Black

let termsize = function
    CondBranch _ -> 12
  | Call _ -> 12
  | Jump _ -> 8
  | Return -> 4

let blocksize b =
  let Block(l,term) = b in
  4*(List.length l) + termsize term

let apply_blockref_as_ast fn acc blkref =
  match blkref with
    Indirect ast -> fn acc ast
  | _ -> acc

let apply_term_as_ast_forwards fn acc term =
  match term with
    CondBranch(_,cond,trueblk,falseblk) ->
      let acc = fn acc cond in
      let acc = apply_blockref_as_ast fn acc trueblk in
      apply_blockref_as_ast fn acc falseblk
  | Call(toblk,retblk) ->
      let acc = apply_blockref_as_ast fn acc toblk in
      apply_blockref_as_ast fn acc retblk
  | Jump(toblk) ->
      apply_blockref_as_ast fn acc toblk
  | Return -> acc

let block_fold_forwards fn acc block =
  let Block(astlist,term) = block in
  let acc = list_fold_forwards fn acc astlist in
  apply_term_as_ast_forwards fn acc term

let apply_term_as_ast_backwards fn term acc =
  let fn' a b = fn b a in
  match term with
    CondBranch(_,cond,trueblk,falseblk) ->
      let acc = apply_blockref_as_ast fn' acc falseblk in
      let acc = apply_blockref_as_ast fn' acc trueblk in
      fn cond acc
  | Call(toblk,retblk) ->
      let acc = apply_blockref_as_ast fn' acc retblk in
      apply_blockref_as_ast fn' acc toblk
  | Jump(toblk) ->
      apply_blockref_as_ast fn' acc toblk
  | Return -> acc

let block_fold_backwards fn block acc =
  let Block(astlist,term) = block in
  let acc = apply_term_as_ast_backwards fn term acc in
  list_fold_backwards fn astlist acc

let map_blockref_as_ast fn blkref =
  match blkref with
    Indirect ast -> Indirect(fn ast)
  | _ -> blkref

let map_postorder fn block =
  let Block(astlist,term) = block in
  let astlist' = List.map (fun ast -> Ast.map_postorder fn ast) astlist in
  let term' =
    match term with
      CondBranch(prob, cond, trueblk, falseblk) ->
        let cond' = Ast.map_postorder fn cond
        and trueblk' = map_blockref_as_ast fn trueblk
        and falseblk' = map_blockref_as_ast fn falseblk in
        CondBranch(prob, cond', trueblk', falseblk')
    | Call(toblk, retblk) ->
        let toblk' = map_blockref_as_ast fn toblk
        and retblk' = map_blockref_as_ast fn retblk in
        Call(toblk', retblk')
    | Jump(toblk) ->
        let toblk' = map_blockref_as_ast fn toblk in
        Jump(toblk')
    | Return -> term
  in
    Block(astlist', term')
