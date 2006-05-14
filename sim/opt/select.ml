(* Instruction selection by dynamic programming *)

open Ast
open Memotree
open Mtree

exception NoMatches

module S = Sets.RegOrPseudoSet

type selectedast = {
  mutable s_asm: insnorphi;
  s_def: Id.reg_or_pseudo list;
  s_use: Id.reg_or_pseudo list;
  mutable s_live_in: S.t;
  mutable s_live_out: S.t;
  s_num: int
}

type selectedinsns = Ast.ast * Ast.ast list

(*let countchildren memotree =
  Memotree.fold_left
    (fun acc m -> match m with
        MMatch(m, _) -> acc+m.cost
      | _ -> acc)
    0 memotree*)

(* Should treat int/float nodes differently *)
(* let betternode oldmemo newcost newasm newasteq newred newkids =
  match oldmemo with
    MMatch(omem, orig) as x ->
      if newcost < omem.cost then
        MMatch({cost=newcost; asm=newasm;
                reduce=newred; children=newkids;
                astequiv=newasteq}, 
               orig)
      else
        x
  | _ -> MMatch({cost=newcost; asm=newasm;
                 reduce=newred; children=newkids;
                 astequiv=newasteq}, 
                oldmemo) *)

(* This just iterates through all the available rules, trying each in turn
   and keeping the best match found so far. A better solution would be to use
   a FSM to do the matching, which would waste far less time (but give the
   same results). Doing that would make the rule parser far more complex,
   something more like a rule compiler itself...
*)
(* let rec matching rules ast memo =
  match rules with
    [] -> memo
  | (cost,rule,asm)::rs ->
      let env = Hashtbl.create 5 in
      try
        let rewrite =
          let thiscost = rule env ast memo
            {m_cost=cost; m_children=[]; m_reduction=NoMatch; m_ast=Nop Noop}
          in
            betternode memo thiscost.m_cost (fun alloc -> asm env alloc) 
              thiscost.m_ast thiscost.m_reduction thiscost.m_children
        in
         (* Printf.printf
            "Rule matches, ast %s\norig memo %s, repl memo %s\n"
            (Disast.writeop ast)
            (Memotree.string_of_memotree memo)
            (Memotree.string_of_memotree rewrite); *)
            matching rs ast rewrite
      with
        Fail -> matching rs ast memo *)

exception NotFoundOptimum of ast

(* Find an optimum tiling for an AST node, "bottom-up". This is the part
   which does instruction selection by dynamic programming.
   Some confusion comes from how the Memotree is used for both an argument
   and a result from the "matching" function: matching kind of filters the
   value, filling in costs + matching nodes. This value is then passed up
   towards the root of the tree.
*)
(* let rec findoptimum rules ast =
  let memo = begin match Mtree.denoted ast with
    Triop(opcode,op1,op2,op3) ->
      let a = findoptimum rules op1 in
      let b = findoptimum rules op2 in
      let c = findoptimum rules op3 in
      MTri(a,b,c)
  | Binop(opcode,op1,op2) ->
      let a = findoptimum rules op1 in
      let b = findoptimum rules op2 in
      MBin(a,b)
  | Unop(opcode,child) ->
      MUn(findoptimum rules child)
  | Nop(opcode) ->
      MNull
  | Move(lhs,rhs) ->
      let a = findoptimum rules lhs in
      let b = findoptimum rules rhs in
      MBin(a,b)
  | Pair(a,b) ->
      let a = findoptimum rules a in
      let b = findoptimum rules b in
      MBin(a,b)
  | Register _ -> MNull
  | Pseudo _ -> MNull
  | Constant _ -> MNull
  | _ -> raise (NotFoundOptimum ast)
  end in
    matching rules ast memo *)

let iterate_rules ast =
  let rec scan bestsofar best_ast best_kids =
    (* Printf.fprintf stderr "Matching %s: best so far=%s\n"
      (Disast.writeop ast)
      (if bestsofar=max_int then "inf" else string_of_int bestsofar);
    flush stderr; *)
    try
      let ast, kids = Rules.rules bestsofar ast in
      match ast with
        Reduction { Ast.ruleno = rno; cost = cst } ->
          if cst < bestsofar then begin
        (*  Printf.fprintf stderr "Better rule found (%d), cost %d\n" rno cst;*)
            scan cst ast kids
          end else begin
        (*  Printf.fprintf stderr "Nothing better found\n"; *)
            best_ast, best_kids
          end
      | _ ->
        (* Printf.fprintf stderr "Nothing found\n"; flush stderr; *)
        best_ast, best_kids
    with Match_failure _ ->
      (* Printf.fprintf stderr "Matching failed\n"; *)
      best_ast, best_kids
  in
    scan max_int ast []

exception SelectFailed of ast

let select ast =
  let rec scan ast kids =
    let ast',kids' =
      match ast with
        Triop(opcode, op1, op2, op3) ->
          let op1', kids = scan op1 kids in
          let op2', kids = scan op2 kids in
          let op3', kids = scan op3 kids in
          Triop(opcode, op1', op2', op3'), kids
      | Binop(opcode,op1,op2) ->
          let op1', kids = scan op1 kids in
          let op2', kids = scan op2 kids in
          Binop(opcode, op1', op2'), kids
      | Unop(opcode,op1) ->
          let op1', kids = scan op1 kids in
          Unop(opcode, op1'), kids
      | Move(a,b) ->
          let a', kids = scan a kids in
          let b', kids = scan b kids in
          Move(a', b'), kids
      | x -> x, kids
    in
      let ast', morekids = iterate_rules ast' in
      ast', morekids @ kids'
  in
    let ast, kids = scan ast [] in
    match ast with
      Reduction _ -> ast, kids
    | _ -> raise (SelectFailed ast)

(* let select rules ast =
  let topmatch m =
    match m with
      MMatch(x, _) -> x
    | _ ->
      Printf.printf "Select failed for node: %s\n" (Disast.writeop ast);
      failwith "exit"
  in try
    topmatch (findoptimum rules ast)
  with (NotFoundOptimum ast1) ->
    print_endline ("Failed node: " ^ Disast.writeop ast ^
                   "\nAt subnode: " ^ Disast.writeop ast1);
    failwith "Instruction selection failed"

(* Split original AST and the selected instruction memos into a list of
   AST nodes, for subsequent register allocation.
*)
let split memo =
  let rec scan memols acc =
    match memols with
      [] -> acc
    | memo::rest ->
        let node = {
          s_ast = memo.astequiv;
          s_asm = memo.asm;
        }
        in
          scan memo.children (scan rest (node::acc))
  in
    scan [memo] []

(* let insnise q =
  let rec make w acc =
    match w with
      [] -> acc
    | w::ws -> make w.children (make ws (w.asm () :: acc))
  in
    List.flatten (make [q] []) *)

*)

let accum_selected_insns selection acc num =
  let (first,kids) = selection in
  let insns = List.rev (first::kids) in
  List.fold_right
    (fun red (acc,num) ->
       match red with
         Reduction { Ast.insn = iorp; defs = defs; uses = uses } ->
           let defs_rop = List.map Ast.reg_or_pseudo_of_ast defs
           and uses_rop = List.map Ast.reg_or_pseudo_of_ast uses in
           (* Fixed assignments count as uses: the "use" is the rest of the
              program.  *)
           let uses_rop = List.fold_right
             (fun def ulist ->
               match def with
                 Register(_, _, FixedAssign) ->
                   Ast.reg_or_pseudo_of_ast def :: ulist
               | _ -> ulist)
             defs
             uses_rop in
           (* Printf.printf "insns: defs %s; uses %s\n"
             (String.concat "," (List.map Id.string_of_rop defs_rop))
             (String.concat "," (List.map Id.string_of_rop uses_rop)); *)
           ({ s_asm = iorp;
              s_def = defs_rop; s_use = uses_rop;
              s_live_in = S.empty; s_live_out = S.empty; s_num = num } :: acc,
            num + 10)
       | _ -> (acc, num))
    insns
    (acc, num)

let insns_for_ast ast startnum =
  let selected = select ast in
  let insns, _ = accum_selected_insns selected [] startnum in
  insns

let accum_insns_for_ast ast acc num =
  let selected = select ast in
  accum_selected_insns selected acc num

let fill_alloc beans =
  List.fold_right
    (fun (src,target) al -> Sets.RegOrPseudoMap.add src target al)
    beans
    Sets.RegOrPseudoMap.empty
    
let alloc_a = fill_alloc
  [Id.PhysReg((5, Id.Suf 0), Id.IntType),
     Id.PhysReg((5, Id.Unset), Id.IntType);
   Id.PhysReg((4, Id.Unset), Id.IntType),
     Id.PhysReg((4, Id.Unset), Id.IntType);
   Id.PhysReg((60, Id.Unset), Id.IntType),
     Id.PhysReg((60, Id.Unset), Id.IntType);
   Id.PhysReg((60, Id.Suf 0), Id.IntType),
     Id.PhysReg((60, Id.Unset), Id.IntType);
   Id.PhysReg((22, Id.Unset), Id.IntType),
     Id.PhysReg((22, Id.Unset), Id.IntType);
   Id.PseudoReg(1, Id.IntType),
     Id.PhysReg((23, Id.Unset), Id.IntType);
   Id.PseudoReg(2, Id.IntType),
     Id.PhysReg((24, Id.Unset), Id.IntType);
   Id.PseudoReg(3, Id.IntType),
     Id.PhysReg((25, Id.Unset), Id.IntType);
   Id.PseudoReg(4, Id.IntType),
     Id.PhysReg((26, Id.Unset), Id.IntType);
   Id.PseudoReg(5, Id.IntType),
     Id.PhysReg((27, Id.Unset), Id.IntType);
   Id.PseudoReg(6, Id.IntType),
     Id.PhysReg((28, Id.Unset), Id.IntType);
   Id.PseudoReg(7, Id.IntType),
     Id.PhysReg((29, Id.Unset), Id.IntType);
   Id.PseudoReg(8, Id.IntType),
     Id.PhysReg((30, Id.Unset), Id.IntType);
   Id.PseudoReg(9, Id.IntType),
     Id.PhysReg((31, Id.Unset), Id.IntType);
   Id.PseudoReg(10, Id.IntType),
     Id.PhysReg((32, Id.Unset), Id.IntType);
   Id.PseudoReg(11, Id.IntType),
     Id.PhysReg((33, Id.Unset), Id.IntType);
   Id.PseudoReg(12, Id.IntType),
     Id.PhysReg((34, Id.Unset), Id.IntType)]

let alloc_b = fill_alloc
  [Id.PhysReg((1, Id.Unset), Id.IntType),
     Id.PhysReg((1, Id.Unset), Id.IntType);
   Id.PhysReg((2, Id.Unset), Id.IntType),
     Id.PhysReg((2, Id.Unset), Id.IntType);
   Id.PhysReg((3, Id.Unset), Id.IntType),
     Id.PhysReg((3, Id.Unset), Id.IntType);
   Id.PhysReg((22, Id.Unset), Id.IntType),
     Id.PhysReg((20, Id.Unset), Id.IntType);
   Id.PseudoReg(1, Id.IntType),
     Id.PhysReg((21, Id.Unset), Id.IntType);
   Id.PseudoReg(2, Id.IntType),
     Id.PhysReg((22, Id.Unset), Id.IntType);
   Id.PseudoReg(3, Id.IntType),
     Id.PhysReg((23, Id.Unset), Id.IntType);
   Id.PseudoReg(4, Id.IntType),
     Id.PhysReg((24, Id.Unset), Id.IntType);
   Id.PseudoReg(5, Id.IntType),
     Id.PhysReg((25, Id.Unset), Id.IntType);
   Id.PseudoReg(6, Id.IntType),
     Id.PhysReg((26, Id.Unset), Id.IntType)]

let a = Move(Register((5, Id.Suf 0), Id.IntType, Assign),
          Binop(Add, Register((4, Id.Unset), Id.IntType, LastUse),
                     Binop(Mul, Unop(Ind Word, Binop(Add,
                                  Register((60, Id.Suf 0), Id.IntType, Use),
                                  Constant 32l)),
                                Unop(Ind Word, Binop(Add,
                                  Constant 16l,
                                  Register((60, Id.Suf 0), Id.IntType, Use))))))
;;

let b = Move(Register((1, Id.Unset), Id.IntType, Assign),
             Binop(Add, Register((2, Id.Unset), Id.IntType, Use),
                        Register((3, Id.Unset), Id.IntType, Use)))
;;

let c = Move(Register((1, Id.Unset), Id.IntType, Assign),
             Binop(Add, Register((2, Id.Unset), Id.IntType, Use),
                        Constant 16l))
;;

let d = Triop(Branch, Register((1, Id.Unset), Id.IntType, Use),
                      Constant 128l, Constant 160l)
;;

let e = Move(Register((1, Id.Unset), Id.IntType, Assign),
             Binop(Add, Register((2, Id.Unset), Id.IntType, Use),
                        Constant (-16l)))
;;

let f = Move(Register((5, Id.Suf 0), Id.IntType, Assign),
          Unop(Ind Word, Binop(Add, Register((60, Id.Unset), Id.IntType, Use),
                                    Constant (-16l))))
;;

(*let insnise c = List.rev (List.flatten (Memotree.fold_right
  (fun a b -> match a with MMatch(c,_) -> c.asm ()::b | _ -> b) c []))*)

(*let rec emit astm =
  let others = List.map emit astm.children in
  try match astm.memo with
    Unmatched -> raise (SelectFailed astm)
  | IntNode(asm) -> List.flatten others @ asm ()
  | FloatNode(asm) -> List.flatten others @ asm ()
  with SelectFailed a ->
    Printf.fprintf stderr "Failed node: %s\n"
      (Disast.writeop (Ast.unmemoizedast a));
    raise SoImGivingUp

let rec writeselected astm =
  let others = List.map writeselected astm.children in
  print_endline (Disast.writeop (Ast.unmemoizedast astm))

(* perform instruction selection on an AST node and generate code
 * (in internal form)
 *)
let select rules astm =
  try
    findoptimum rules astm;
    writeselected astm;
    emit astm
  with (NotFoundOptimum x) ->
    Printf.fprintf stderr "Failed node: %s\n"
      (Disast.writeop (Ast.unmemoizedast x));
    failwith "Die"

let dumpenv astm =
  match astm.memo with
    IntNode(asm,env) ->
      Hashtbl.iter (fun k v -> Printf.printf "Binding %s\n" k) env
  | _ -> ()

*)
