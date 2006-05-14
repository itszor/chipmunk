open Ast
open Span

(* Recover usable register spans from an AST list
   Spans either conditionally terminate the block (Term),
   or not (Nonterm)
*)

(*type rangetype = Term of int * int
               | Top of Id.id * int * int
               | Bottom of Id.id * int * int*)

(* Finds ORDERED list *)
let subblocks asts =
  let out,_,_ =
    List.fold_left (fun start ast ->
      Ast.fold_rtol_postorder
        (fun node acc ->
          let ls,mark,ctr = acc in
          match node with
            Ast.Binop(Call,_,_)
          | Ast.Triop(Branch,_,_,_)
          | Ast.Binop(Trap _,_,_)
          | Ast.Unop(Jump,_)
          | Ast.Nop(Return) -> (mark,ctr)::ls, ctr+1, ctr+1
          | _ -> ls, mark, ctr+1)
      ast
      start)
    ([], 0, 0)
    asts
  in
    List.rev out

let printsubblocks subs =
  List.iter (fun (p,q) -> Printf.printf "%d-%d\n" p q) subs

(* Return a set of hard reg nos which born & die unbroken in a given span *)
let regsinspan spans st en =
  Printf.printf "Finding spans from %d to %d\n" st en;
  Sets.SpanMap.fold
    (fun id span acc ->
      match span.born,span.dies,span.broken with
        Some x,Some y,false when x>=st && x<=en && y>=st && y<=en ->
          begin match id with
            Id.PhysRegi(num,_) -> Sets.IntSet.add num acc
          | _ -> acc
          end
      | _ -> acc)
    spans
    Sets.IntSet.empty

let definitelydead ranges spans =
  List.map (fun (st,en) -> en, regsinspan spans st en) ranges

let printdefdead d =
  let print_live l = Sets.IntSet.iter (fun x -> Printf.printf " %d" x) l in
  List.iter (fun (en,m) -> print_int en; print_live m; print_newline ()) d

(* Inserting note is strange because ast_of_seq_* expects to pull its
 * AST node from the following sequence item, not the current one. Left
 * like that for consistency elsewhere...
 *)
let rewrite asts rewritelist =
  let _,_,newastlist = List.fold_left
    (fun start ast ->
      let seq = Ast.seq_of_ast_rl ast in
      let ctr,rewrites,alist = start in
      let ctr,rewrote,nseq = Seq.fold_right
        (fun node (ctr,lives,res) ->
          match lives with
            (en,m)::ls when en=ctr ->
              let exph = {exp_ints=m; exp_flts=Sets.IntSet.empty} in
              ctr+1, ls,
                Seq.cons (Note(ExpireHere exph, Ast.Nop Ast.Noop))
                  (Seq.cons node res)
          | _ -> ctr+1, lives, Seq.cons node res)
        seq
        (ctr,rewrites,Seq.Empty)
      in
        ctr,rewrote,(Ast.ast_of_seq_rl nseq)::alist)
    (0, rewritelist, [])
    asts
  in
    List.rev newastlist

(*let recover asts free =
  List.fold_left (fun feed ast ->
    Ast.fold_right_postorder
      (fun node feed ->
        let dd,ctr = feed in
        match dd with
          [] -> node
        | (en,free)::rest ->*)

(*type partinfo = Born of Id.id * int
              | Dies of Id.id * int*)

(* For ids which come alive or die partway through a subblock, whose
   scope extends before or after that subblock, we will split the subblock.
   There will be a note at the last (inclusive) place that a particular set
   of registers is valid, so in the "born" case we ask for the note to be
   before. The value of -1 might put notes in funny places though...
*)
(*let partial spans =
  let unordered =
    Sets.SpanMap.fold
      (fun id span acc ->
        match span.born,span.dies with
          Some b, None -> (Born (id,b-1))::acc
        | None, Some d -> (Dies (id,d))::acc
        | _ -> acc)
      spans
      []
  in let pos a =
    match a with Born(_,n) -> n | Dies(_,n) -> n
  in
    List.sort (fun a b -> compare (pos a) (pos b)) unordered

let print p = List.iter (fun p -> Printf.printf "%d\n" p) p*)

(* Take asts and spans, return a list of split subblocks *)
(*let splitblocks asts spans =
  let partial = partial spans
  and subblocks = subblocks asts in
  let rec scan span partial =
    match span,partial with
      [],[] -> []
    | [],_ -> []
    | s,[] -> s
    | (Term (blkst,blken))::spans,pp::parts
    | (Top (_,blkst,blken))::spans,pp::parts
    | (Bottom (_,blkst,blken))::spans,pp::parts ->
        let p = match pp with Born(_,p) -> p | Dies(_,p) -> p in
        if p>blkst && p<blken then
          match pp with
            Born(id,p) ->
              (Top (id,blkst,p))::(scan ((Term (p+1,blken))::spans) parts)
          | Dies(id,p) ->
              (Bottom (id,blkst,p))::(scan ((Term (p+1,blken))::spans) parts)
        else
          (Term (blkst,blken))::(scan spans (pp::parts))
  in
    scan subblocks partial*)

(* Rewrite AST list with notes, saying which registers can be used up to
   & including the node the note is attached to (from the previous note, 
   noninclusive).
*)
(*let recover asts spans ranges =
  let res,_,_,_,_ =
    List.fold_left (fun feed ast ->
      Ast.fold_right_postorder
        (fun node feed ->
          let out,ranges,pend,mark,ctr = feed in
          match ranges with
            [] -> out,ranges,pend,mark,ctr+1
          | Term(st,en)::rs when en==ctr ->
              let live = Sets.IntSet.union (regsinspan spans mark ctr) pend in
              (en,live)::out,rs,Sets.IntSet.empty,ctr+1,ctr+1
          | Top(id,st,en)::rs when en==ctr ->
              let num,_ = id in
              let live = Sets.IntSet.add num pend in
              (en,live)::out,rs,pend,mark,ctr+1
          | Bottom(id,st,en)::rs when en==ctr ->
              let num,_ = id in
              let npend = Sets.IntSet.add num pend in
              (en,Sets.IntSet.empty)::out,rs,npend,mark,ctr+1
          | _ -> out,ranges,pend,mark,ctr+1)
      ast
      feed)
    ([],ranges,Sets.IntSet.empty,0,0)
    asts
  in
    res
  
let printrecovered x =
  let print_live l = Sets.IntSet.iter (fun x -> Printf.printf " %d" x) l in
  List.iter (fun (en,live) ->
    Printf.printf "at %d:" en;
    print_live live;
    print_newline ()) x
*)
