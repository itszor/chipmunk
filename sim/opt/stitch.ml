(* Attempt to join two blocks together *)

(* Should generalise to making a "fragment" of code. A fragment is triggered
   by a block A jumping to another block B, still. Now the rules should be:
   
   A:cbr -> if probability highly biased, follow likely branch only, otherwise
            stop here.
   A:jump -> follow branch & recur
   A:call -> follow branch & recur
   A:ret -> depends on whether link register is constant?
   
   This routine and the optimiser (ie, constant propagation) are
   co-dependent. Ret should probably stop here initially, then we
   can be called again after optimisation if beneficial.
   
   OTOH, we can probably tell "syntactically" whether link reg is written
   to, and thus follow ret here. That might be more efficient in the long
   run.
   
   At this stage, we can also fold transitive branches, maybe?
   
   *FIXME* because of the definition of return (stack semantics), we can be
   safe (probably) following RET as long as we have a CALL higher up in the
   trace... (and it will return to the Y part of the call.)
*)

open Common
open Block
open I32op

let constant_p = function
    Direct(x) -> true
  | _ -> false

let is_p dest blkref =
  match dest with
    Direct(x) -> x=blkref
  | _ -> false

let listofinsn mem proc blk =
  let rec uptoterm addr =
    let raw = mem#readWord addr in
    let opcode = Int32.to_int (Int32.shift_right_logical raw 26) in
    if opcode<48 || opcode>51 then
      raw :: (uptoterm (addr +! 4l))
    else
      [raw]
  in
    let base = (Lookup.lookup mem proc blk) in
    uptoterm base

(* The literal translation of a block into AST *)
let makeblock mach blk =
  listofinsn mach#getmem mach#getproc blk ++
  List.map Decode.decode ++
  Translate.translist ++
  Mergeldt.mergeldt

(* let committed_regs =
  let rec gen_list' make num =
    match num with
      64 -> []
    | num -> make num :: gen_list' make (num+1)
  in let gen_list make = gen_list' make 0 in
    [Ast.Vector(64, gen_list (fun n ->
       Ast.Move(Ast.Register((n, Id.Unset), Ast.FixedAssign),
                Ast.Register((n, Id.Unset), Ast.Use))));
     Ast.Vector(64, gen_list (fun n ->
       Ast.Move(Ast.Floatreg((n, Id.Unset), Ast.FixedAssign),
                Ast.Unop(Ast.Cast_f Iformat.Double,
                  Ast.Floatreg((n, Id.Unset), Ast.Use)))))]
*)

(* Make a local blockref to exit through when leaving our local fragment,
 * which commits all the physical registers from their possibly-later-virtual
 * counterparts.
 *)
let exitwith dest gen_name vertex =
  let block = Block([Ast.Syncregs], dest) in
  let tag =
  {
    domfront = [];
    parent = None;
    predecessors = [];
    successors = [];
    semi = None;
    idom = None;
    idomchild = [];
    ancestor = None;
    samedom = None;
    bucket = [];
    dfnum = -1;
    refno = !gen_name;
    selected = [];
    block = block;
    writeidx = None;
    live_in = Sets.RegOrPseudoSet.empty;
    live_out = Sets.RegOrPseudoSet.empty
  }
  in
    Hashtbl.add vertex !gen_name tag;
    gen_name := Int32.pred !gen_name;
    Local tag

(* Return a Block tag representing the root of a suitable fragment for
 * optimisation.
 *)
let buildfrag mach whichblock =
  let vertex = Hashtbl.create 17
  and gen_name = ref (-1l) in
  let rec nodup dest =
    try
      let exists = Hashtbl.find vertex dest in
      Local (exists)
    with Not_found ->
      Local (buildfrag' dest)
  and buildfrag' blkref =
    let Block(insns, term) as blk = makeblock mach blkref in
    let tag =
    {
      domfront = [];
      parent = None;
      predecessors = [];
      successors = [];
      semi = None;
      idom = None;
      idomchild = [];
      ancestor = None;
      samedom = None;
      bucket = [];
      dfnum = -1;
      refno = blkref;
      selected = [];
      block = blk;
      writeidx = None;
      live_in = Sets.RegOrPseudoSet.empty;
      live_out = Sets.RegOrPseudoSet.empty
    }
    in Hashtbl.add vertex blkref tag;
    let term' = match term with
      Jump (Direct dest) ->
        Jump (nodup dest)
    | Call (Direct func, ((Direct link) as dl)) ->
        Call (nodup func, dl)
    | CondBranch (_, reg, ((Direct trueblk) as tb),
                          ((Direct falseblk) as fb)) ->
        let prof = mach#getserv#getprofiler in
        let bias = prof#get_cbr blkref in
        if bias < 0.3 then
          (* biased towards the truth *)
          CondBranch (bias, reg, nodup trueblk, nodup falseblk (*fb*))
        else if bias > 0.7 then
          (* biased towards lies & deception *)
          CondBranch (bias, reg, (*tb*) nodup trueblk, nodup falseblk)
        else
          (* lib dem supporter *)
          CondBranch (bias, reg, exitwith (Jump tb) gen_name vertex,
                      exitwith (Jump fb) gen_name vertex)
    | x -> Jump(exitwith x gen_name vertex)
    in
      tag.block <- Block(insns, term'); tag
  in
    buildfrag' whichblock

exception PartialReference

let renumber_term term intlocs =
  match term with
    CondBranch(prob, Ast.Register((num,loc), typ, life), x, y) ->
      CondBranch(prob, Ast.Register((num, intlocs.(num)), typ, life), x, y)
  | x -> x

(* Uses an 'interesting' version of List.map because we need to apply
   the scan function in order because we rely on side effects.
   Ideally should use entirely functional implementation instead.
*)
(*let renumber astlist term =
  let intlocs = Array.create 64 Id.Initial
  and fltlocs = Array.create 64 Id.Initial in
  let bump arr num =
    match arr.(num) with
      Id.Initial -> arr.(num) <- Id.Line 0
    | Id.Line n -> arr.(num) <- Id.Line (n+1)
    | _ -> ()
  in let rec rewritevector vec =
    match vec with
      [] -> []
    | v::vs ->
        let newreg = match v with
          Ast.Register((num,loc),expi) ->
            bump intlocs num;
            Ast.Register((num, intlocs.(num)), expi)
        | Ast.Floatreg((num,loc),expi) ->
            bump fltlocs num;
            Ast.Floatreg((num, fltlocs.(num)), expi)
        | _ -> failwith "Bad register in vector"
        in
          newreg :: rewritevector vs
  in let rec scan = function
    Ast.Triop(op,a,b,c) ->
      let aa = scan a in
      let bb = scan b in
      let cc = scan c in
        Ast.Triop(op,aa,bb,cc)
  | Ast.Binop(op,a,b) ->
      let aa = scan a in
      let bb = scan b in
        Ast.Binop(op,aa,bb)
  | Ast.Unop(op,a) -> Ast.Unop(op, scan a)
  | Ast.Nop(op) as x -> x
  | Ast.Pair(a,b) ->
      let aa = scan a in
      let bb = scan b in
        Ast.Pair(aa,bb)
  | Ast.Vector(l,v) -> Ast.Vector(l,List.map scan v)
  | Ast.Register((num,loc),expi) -> Ast.Register((num, intlocs.(num)), expi)
  | Ast.Floatreg((num,loc),expi) -> Ast.Floatreg((num, fltlocs.(num)), expi)
  | Ast.Move(a,b) ->
      let bb = scan b in
      let aa = 
        match Lvalue.classify a with
          Lvalue.LInt((num,_),expi)
        | Lvalue.LPartInt((num,_),expi) as cls ->
            bump intlocs num;
            Lvalue.rewritereg a cls (Ast.Register((num, intlocs.(num)), expi))
        | Lvalue.LFloat((num,_),expi)
        | Lvalue.LDouble((num,_),expi) as cls ->
            bump fltlocs num;
            Lvalue.rewritereg a cls (Ast.Floatreg((num, fltlocs.(num)), expi))
        | Lvalue.LNone
        | Lvalue.LMemory -> scan a
        | Lvalue.LVector(v) ->
            let rl = rewritevector v in
            Ast.Vector(List.length rl, rl)
      in
        Ast.Move(aa,bb)
  | x -> x
  in
    let newasts = List.rev_map scan (List.rev astlist) in
    Block(newasts, renumber_term term intlocs)

let ast_of_blockref = function
    Indirect r -> Ast.Register r
  | Direct i -> Ast.Constant i
  | _ -> raise PartialReference

let trapconvert reg a b tcond otherwise =
  let trap = Ast.Binop(Ast.Trap(tcond),
                       Ast.Register reg,
                       ast_of_blockref otherwise) in
  let Block(a_insns, a_term) = a
  and Block(b_insns, b_term) = b in
  renumber (b_insns @ (trap :: a_insns)) b_term

let stitch mach primary_ref primary secondary_ref secondary =
  let Block(a_insns, a_term) = primary in
  match a_term with
    Jump(dest) when constant_p dest ->
      let Block(b_insns, b_term) = secondary in
      (Some Tag.Tag_jump_folded, [|primary_ref; secondary_ref|],
        renumber (b_insns @ a_insns) b_term)

  | CondBranch(prob, reg, a_blk, b_blk) when constant_p a_blk &&
                                       is_p a_blk secondary_ref ->
      (Some Tag.Tag_trap_converted, [|secondary_ref; primary_ref|],
        trapconvert reg primary secondary Iformat.OnZero b_blk)

  | CondBranch(prob, reg, a_blk, b_blk) when constant_p b_blk &&
                                       is_p b_blk secondary_ref ->
      (Some Tag.Tag_trap_converted, [|primary_ref; secondary_ref|],
        trapconvert reg primary secondary Iformat.OnNonzero a_blk)

  | _ -> (None, [||], primary)
*)
