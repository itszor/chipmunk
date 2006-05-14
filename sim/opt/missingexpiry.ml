(* Do a backward pass over an AST block to mark any registers which should
   be dead but aren't
*)

open Ast
open Block

let missing_expiry blk =
  let scan asts = List.fold_left
    (fun start ast ->
      let seq = Ast.seq_of_ast_lr ast in
      let si,sf,alist = start in
      let di,df,nseq = Seq.fold_right
        (fun node acc ->
          let deadints,deadflts,res = acc in
          match node with
            Register((num,loc) as id,life) ->
              begin match life with
                Assign ->
                  Sets.IntSet.add num deadints, deadflts, Seq.cons node res
              | Use ->
                  if Sets.IntSet.mem num deadints then
                    Sets.IntSet.remove num deadints,
                    deadflts,
                    Seq.cons (Register(id, LastUse)) res
                  else
                    deadints, deadflts, Seq.cons node res
              | LastUse ->
                  Sets.IntSet.remove num deadints,
                  deadflts,
                  Seq.cons node res
              end
          | Floatreg((num,loc) as id,life) ->
              begin match life with
                Assign ->
                  deadints, Sets.IntSet.add num deadflts, Seq.cons node res
              | Use ->
                  if Sets.IntSet.mem num deadflts then
                    deadints,
                    Sets.IntSet.remove num deadflts,
                    Seq.cons (Floatreg(id, LastUse)) res
                  else
                    deadints, deadflts, Seq.cons node res
              | LastUse ->
                  deadints,
                  Sets.IntSet.remove num deadflts,
                  Seq.cons node res
              end
          | _ -> deadints, deadflts, Seq.cons node res)
        seq
        (si,sf,Seq.Empty)
      in
        di,df,(Ast.ast_of_seq_lr nseq)::alist)
    (Sets.IntSet.empty, Sets.IntSet.empty, [])
    asts
  in
    let Block(astlist, term) = blk in
    let _,_,newastlist = scan astlist in
    Block(List.rev newastlist, term)
