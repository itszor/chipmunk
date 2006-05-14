open Block

(* The algorithm for inserting PHI-nodes is as follows.
   (from "Modern compiler implementation in ML", Andrew Appel)
   (...with errata fixed)

  Place-PHI-Functions =
    for each node n
      for each variable a in Aorig[n]
        defsites[a] <- defsites[a] U {n}
    for each variable a
      W <- defsites[a]
        while W not empty
          remove some node n from W
          for each Y in DF[n]
            if a not in Aphi[Y]
              insert statement a <- PHI(a, a, ..., a) at top of
                block Y, where the PHI-function has as many arguments
                as Y has predecessors
              Aphi[Y] <- Aphi[Y] U {a}
              if a not in Aorig[Y]
                W <- W U {Y}

  U : union
  W : linked list (worklist)
  Aphi[n] : A (subscript) phi - set of variables with phi functions at node n
  Aorig[n] : A (subscript) orig - set of variables defined in node n
  G : graph of nodes (for each node n...)
  
  For testing membership of W: mark bit in representation of each node n.

*)

(* Add a block to the places a particular id is referenced. *)
let union defsites id blocknum =
  try
    let bits = Sets.RegOrPseudoMap.find id defsites in
    let bits' = Sets.IntSet.add blocknum bits in
    Sets.RegOrPseudoMap.add id bits' defsites
  with Not_found ->
    let bits = Sets.IntSet.singleton blocknum in
    Sets.RegOrPseudoMap.add id bits defsites

let member defsites id blocknum =
  try
    let defbits = Sets.RegOrPseudoMap.find id defsites in
    Sets.IntSet.mem blocknum defbits
  with Not_found ->
    false

let ids_in_block block acc =
  Block.block_fold_forwards
    (fun acc ast ->
      match ast with
        Ast.Register(id, typ, _) ->
          Sets.RegOrPseudoSet.add (Id.PhysReg(id, typ)) acc
      | _ -> acc)
    acc
    block

let all_ids vertices =
  let acc = ref Sets.RegOrPseudoSet.empty in
  for i = 0 to (DynArray.length vertices)-1 do
    let tag = DynArray.get vertices i in
    acc := ids_in_block tag.block !acc
  done;
  !acc

(* Create a virtual root block (for definitions of variables prior to
 * entry of the real root block), and link it backwards & forwards to
 * the real root block, 0.
 *)
let make_root vertices =
  let num = DynArray.length vertices
  and zero = DynArray.get vertices 0 in
  let vroot = {
    domfront = [];
    parent = None;
    predecessors = [];
    successors = [zero];
    semi = None;
    idom = None;
    idomchild = [zero];
    ancestor = None;
    samedom = None;
    bucket = [];
    dfnum = num;
    block = Block([], Jump(Local zero));
    selected = [];
    refno = 0l;
    writeidx = None;
    live_in = Sets.RegOrPseudoSet.empty;
    live_out = Sets.RegOrPseudoSet.empty
  } in
  zero.parent <- Some vroot;
  zero.predecessors <- vroot :: zero.predecessors;
  (* Some other fields might be important! *)
  DynArray.add vertices vroot;
  num

(* All variables are implicitly defined in the start block.
 * The "start" block is a new virtual block which we will number <start>,
 * which contains definitions for all variables prior to the "real" root
 * block.
 *)
let implicit_root_defs vertices defsites start =
  let allids = all_ids vertices in
  Sets.RegOrPseudoSet.fold
    (fun id defsites -> union defsites id start)
    allids
    defsites

(* Ids defined in a list of ast nodes *)
let defined_ids astlist =
  Ast.list_fold_forwards
    (fun acc ast ->
      match ast with
        Ast.Register(id, typ, Ast.Assign)
      | Ast.Register(id, typ, Ast.FixedAssign) ->
          Sets.RegOrPseudoSet.add (Id.PhysReg(id, typ)) acc
      | _ -> acc)
    Sets.RegOrPseudoSet.empty
    astlist

(* We have a DynArray of vertices to iterate over.
   We should build a map (RegOrPseudoMap) from reg_or_pseudos to BitSet
   (=DFS block num)
*)
let defsites vertices vroot =
  let defsites,_ = DynArray.fold_left
    (fun (defsites, i) tag ->
      let Block(insns,term) = tag.block in
      let defined = defined_ids insns in
      let defsites' = Sets.RegOrPseudoSet.fold
        (fun regorpseudo defsites -> union defsites regorpseudo i)
        defined
        defsites
      in
        defsites', i+1)
    (Sets.RegOrPseudoMap.empty, 0)
    vertices
  in
    implicit_root_defs vertices defsites vroot

(* Make a list of numbers from each set bit in a bitset *)
let nums_from_bitset bits =
  Sets.IntSet.elements bits

let rec display_worklist = function
    [] -> Printf.fprintf stderr "\n"
  | m::ms -> Printf.fprintf stderr "node: %d\n" m; display_worklist ms

(* let list_union li n =
  let rec scan thru =
    match thru with
      [] -> n :: li
    | t::ts -> if t=n then li else scan ts
  in
    scan li *)

(* Block.tag DynArray.t -> BitSet.t Sets.RegOrPseudoMap.t -> unit *)

let place vertices defsites =
  let aphi = ref Sets.RegOrPseudoMap.empty in
  Sets.RegOrPseudoMap.iter
    (fun a defsitebits ->
      let worklist = nums_from_bitset defsitebits in
      let rec consume = function
        [] -> ()
      | n::ns ->
          let tag = DynArray.get vertices n in
          let nsr = ref ns in
          List.iter
            (fun y ->
              if not (member !aphi a y.dfnum) then begin
                let Block(insns,term) = y.block in
                let astified_reg = Ast.ast_of_reg_or_pseudo a
                and npred = List.length y.predecessors in
                let ncopies = Array.make npred (astified_reg Ast.Use) in
                y.block <- Block(insns @ [Ast.Move(astified_reg Ast.Assign,
                                          Ast.Phi ncopies)],
                                 term);
                aphi := union !aphi a y.dfnum;
                if not (member defsites a y.dfnum) then begin
                  nsr := y.dfnum :: !nsr
                end;
              end)
            tag.domfront;
          consume !nsr
      in
        consume worklist)
    defsites

(* Algorithm for renaming variables
   also stolen shamelessly from Appel

  Initialisation:
    for each variable a
      Count[a] <- 0
      Stack[a] <- empty
      push 0 onto Stack[a]
      
  Rename(n) =
    for each statement S in block n
      if S is not a phi-function
        for each use of some variable x in S
          i <- top(Stack[x])
          replace the use of x with x_i in S
      for each definition of some variable a in S
        Count[a] <- Count[a] + 1
        i <- Count[a]
        push i onto Stack[a]
        replace definition of a with definition of a_i in S
    for each successor Y of block n
      Suppose n is the jth predecessor of Y
      for each phi-function in Y
        suppose the jth operand of the phi-function is a
        i <- top(Stack[a])
        replace the jth operand with a_i
    for each child X of n
      Rename(X)
    for each definition of some variable a in the original S
      pop Stack[a]
      
  Yuck, this is very imperative for a book on ML!
*)

type countstack = {
  mutable count : int;
  mutable stack : int list;
}

(* the set of ids is easy to find, it is the keys from the defsites PMap *)
let ids defsites =
  Sets.RegOrPseudoMap.map (fun _ -> {count = 0; stack = [0];}) defsites

let stacktop entry =
  match entry.stack with
    [] -> failwith "Empty stack"
  | top::_ -> top

(* Find non-subscripted version of a reg *)
let find_nosub num typ ids =
  try
    Sets.RegOrPseudoMap.find (Id.PhysReg((num, Id.Unset), typ)) ids
  with Not_found ->
    let prefix = match typ with
      Id.IntType -> "r"
    | Id.PtrType -> "p"
    | Id.FloatType -> "f"
    in failwith ("reg not found: " ^ prefix ^ (string_of_int num))

let rewrite_uses ast ids =
  Ast.map_postorder
    (fun node ->
      match node with
        Ast.Register((num, _), typ, (Ast.Use as usage))
      | Ast.Register((num, _), typ, (Ast.LastUse as usage)) ->
          let entry = find_nosub num typ ids in
          Ast.Register((num, Id.Suf (stacktop entry)), typ, usage)
      | x -> x)
    ast

let rewrite_defs ast ids =
  Ast.map_postorder
    (fun node ->
      match node with
        Ast.Register((num, _), typ, (Ast.Assign as usage))
      | Ast.Register((num, _), typ, (Ast.FixedAssign as usage)) ->
          let entry = find_nosub num typ ids in
          entry.count <- entry.count + 1;
          let i = entry.count in
          entry.stack <- i :: entry.stack;
          Ast.Register((num, Id.Suf i), typ, usage)
      | x -> x)
    ast

let rewrite_blkref blkref ids =
  match blkref with
    Indirect ast -> Indirect(rewrite_uses ast ids)
  | x -> x

let rewrite_term term ids =
  match term with
    CondBranch(weight, cond, trueblk, falseblk) ->
      CondBranch(weight, rewrite_uses cond ids,
                         rewrite_blkref trueblk ids,
                         rewrite_blkref falseblk ids)
  | Call(toblk, retblk) -> Call(rewrite_blkref toblk ids,
                                rewrite_blkref retblk ids)
  | Jump(toblk) -> Jump(rewrite_blkref toblk ids)
  | Return -> Return

let rewrite_statements block ids =
  let rec walklist src acc =
    match src with
      [] -> acc
    | ast::asts ->
        let uses_renamed =
          begin match Stmt.classify ast with
            Stmt.PhiFunc -> ast  (* phi node, no rewrite *)
          | _ ->
            rewrite_uses ast ids
          end
        in let defs_renamed = rewrite_defs uses_renamed ids in
        walklist asts (defs_renamed :: acc)
  in let Block(astlist,term) = block in
  let astlist' = walklist (List.rev astlist) [] in
  let term' = rewrite_term term ids in
  Block(astlist', term')

let rewrite_phi_node p j ids =
  let Block(astlist, term) = p.block in
  List.iter
    (fun stmt ->
      match stmt with
        Ast.Move(dest, Ast.Phi nodes) ->
          begin match nodes.(j) with
            Ast.Register((num, loc), typ, usage) ->
              let entry = find_nosub num typ ids in
              nodes.(j) <- Ast.Register((num, Id.Suf (stacktop entry)), typ, 
                                        usage)
          | _ -> ()
          end
      | _ -> ())
    astlist

let seek_nth_predecessor preds blk =
  let rec scan preds' j =
    match preds' with
      [] ->
        Printf.printf "this dfnum=%d\n" blk.dfnum;
        List.iter (fun p -> Printf.printf "predecessor dfnum=%d\n" p.dfnum)
                  preds;
        failwith "Successor/predecessor mismatch"
    | p::ps -> if p==blk then j else scan ps (j+1)
  in
    scan preds 0

let rewrite_phi_nodes block ids =
  let rec walk successors =
    match successors with
      [] -> ()
    | successor :: rest ->
        let j = seek_nth_predecessor successor.predecessors block in
        rewrite_phi_node successor j ids;
        walk rest
  in
    walk block.successors

let pop_defs block ids =
  let Block(astlist,_) = block in
  List.iter
    (fun stmt -> Ast.iter
      (fun node ->
        match node with
          Ast.Register((num,_), typ, Ast.Assign)
        | Ast.Register((num,_), typ, Ast.FixedAssign) ->
            let entry = find_nosub num typ ids in
            entry.stack <- List.tl entry.stack
        | _ -> ())
      stmt)
    astlist

(* Check use of idomchild here! *)
let rename root defsites =
  let vars = ids defsites in
  let rec rename' tag =
    tag.block <- rewrite_statements tag.block vars;
    rewrite_phi_nodes tag vars;
    List.iter rename' tag.idomchild;
    pop_defs tag.block vars
  in
    rename' root

(* Modifies the array "mods" in-place!
   FIXME: This can fail at runtime if a phi node has sometihng other than
   a plain register as one of its sources. Perhaps we should use full-blown
   instruction selection? That could require extra registers to be allocated,
   though.
   I think we should probably disallow putting non-register things in Phi
   nodes.
   FIXME: That's too difficult to do with constant elimination as is works
   currently. Solutions would be (a) "proper" rematerialisation, (b) re-using
   instruction selection machinery here, or (c) doing a hack to be able to
   reload constants ourselves (requiring ISA-dependent code here).
   We should really go for (b).
   (b) can introduce new pseudos in the general case, though our ISA allows
   us to not have to. It'd be nice to be able to ask the instruction selection
   functions to never create new pseudos though, just in case.  *)

let eliminate_phi_in mods selectedlist preds =
  List.fold_right
    (fun i acc ->
      match i.Select.s_asm with
        Ast.Iphi(dest, nodes) ->
          for i=0 to (Array.length nodes)-1 do
            let dtag = preds.(i) in
            let ddfnum = dtag.dfnum in
            let ast = Ast.Move(dest, nodes.(i)) in
            (* FIXME: Here we'd really like to request that no new pseudos
               be created, but we don't. This could lead to crashes later.
               FIXME: -1 will need fixing if we ever need the insn numbers
               for anything (spill code insertion?). *)
            let acc, _ = Select.accum_insns_for_ast ast mods.(ddfnum) (-1) in
            mods.(ddfnum) <- acc
          done;
          acc
      | _ -> i::acc)
    selectedlist
    []

(* Convert phi nodes into moves *)
let eliminate vertices =
  let num = DynArray.length vertices in
  let mods = Array.create num [] in
  for i=0 to num-1 do
    let tag = DynArray.get vertices i in
    let preds = Array.of_list tag.predecessors in
    tag.selected <- eliminate_phi_in mods tag.selected preds
  done;
  (* Insert move instructions at the end of predecessor blocks.  *)
  for i=0 to num-1 do
    let tag = DynArray.get vertices i in
    tag.selected <- tag.selected @ mods.(tag.dfnum)
  done
