open Common
open I32op

(* Chain parts of optimiser together *)

let writelist mem base insns =
  let rec writel addr insns =
    match insns with
      [] -> ()
    | n :: ns -> mem#writeWord addr n; writel (addr +! 4l) ns
  in
    writel base insns

let writedump state name ht ?extra:extr features =
  Printf.printf "Outputting dump `%s'\n" name;
  let file = open_out name in
  Dumps.dump state file ht ?extra:extr features;
  close_out file

(* Constant propagation/evalation loop *)
let rec propeval vertices usedefmap =
  let mods = Constprop.propagate vertices usedefmap in
  Consteval.evalvertices vertices;
  if mods==true then propeval vertices usedefmap

let (@@) a b = b a

  (* let il = List.map (fun s -> Select.insns s alloc) slist in
  let fil = List.flatten il in
  List.iter (fun insn -> print_endline (Disassemble.diss mach insn)) fil *)
  
(*  let slist = List.map (Select.select Rules.rules) tfermerged in
  let split = List.flatten (List.map Select.split slist) in
  tag.Block.selected <- Some split *)

(* let emit mach alloc tag =
  let selected = Option.get tag.Block.selected in
  let ifmt = List.flatten (List.map (fun s -> s.Select.s_asm ()) selected) in
  Printf.printf "block %d:\n" (tag.Block.dfnum);
  List.iter (fun insn -> print_endline ("  " ^ Disassemble.diss mach insn)) ifmt
*)

module S = Sets.RegOrPseudoSet
module M = Sets.RegOrPseudoMap

let write_interference interference file =
  Sets.IntfGraph.fold_node
    (fun node _ _ ->
      let edglist = Sets.IntfGraph.connected node interference in
      Printf.fprintf file "%s: {%s}\n"
        (Id.string_of_rop node)
        (String.concat ", " (List.map Id.string_of_rop edglist));
      ())
    interference
    ()


(* Strip hard-reg setting instructions which haven't changed in our bit of
   code. Dodgy.
   FIXME: Must also remove registers from the available pool!
   FIXME: The Cast_f here is totally redundant. Fix it (elsewhere).  *)
let remove_uninteresting vert =
  DynArray.iter
    (fun vtx ->
      vtx.Block.block <- Block.map_postorder
        (fun ast ->
          match ast with
            Ast.Move(Ast.Register((nd, Id.Suf(sd)), typed, Ast.FixedAssign),
                     Ast.Register((ns, Id.Suf(ss)), types, _))
              when typed=types
                   && sd=1 && ss=0
                   && nd=ns ->
                Ast.Null
          | Ast.Move(Ast.Register((nd, Id.Suf(sd)), typed, Ast.FixedAssign),
                     Ast.Unop(Ast.Cast_f(Iformat.Double),
                       Ast.Register((ns, Id.Suf(ss)), types, _)))
              when typed=types
                   && sd=1 && ss=0
                   && nd=ns ->
                Ast.Null
          | _ -> ast)
        vtx.Block.block)
    vert

(* let regpool =
  List.fold_right
    (fun t set -> Sets.RegOrPseudoSet.add t set)
    [Id.PhysReg((0, Id.Unset), Id.IntType);
     Id.PhysReg((1, Id.Unset), Id.IntType);
     Id.PhysReg((2, Id.Unset), Id.IntType);
     Id.PhysReg((3, Id.Unset), Id.IntType);
     Id.PhysReg((4, Id.Unset), Id.IntType);
     Id.PhysReg((5, Id.Unset), Id.IntType)]
    Sets.RegOrPseudoSet.empty *)

(* Common transition between two blocks.
   In this case we can:
      1. Fold jumps to dest block, making a bigger block
      2. Convert conditional branches to traps
*)
let common_transition mach fromblk toblk =
(*  let tag,data,newblk =*)
  let fragment = Stitch.buildfrag mach fromblk in
  let vertices = Dominator.dfs fragment in
  writedump mach (Printf.sprintf "dump-%ld-%ld.00.dfs.dot" fromblk toblk)
    vertices [`VERTEX; `PRED; `SUCC];
  let fragment' = Simplifyflow.simplify vertices in
  let vertices' = Dominator.dfs fragment' in
  let vertices'' = Dominator.dominators vertices' in
  (* Do "realise hard reg writeback" here *)
  Synchronise.sync vertices'';
  writedump mach (Printf.sprintf "dump-%ld-%ld.01.dominator.dot" fromblk toblk)
    vertices'' [`VERTEX; `PRED; `SUCC];
  Dominator.computedf fragment';
  let vroot = Phi.make_root vertices'' in
  let defsites = Phi.defsites vertices'' vroot in
  Phi.place vertices'' defsites;
  Phi.rename (DynArray.get vertices'' vroot) defsites;
  let usedefmap = Usedef.count vertices'' in
  Deadcode.check_defs usedefmap;
  Deadcode.remove vertices'' usedefmap;
  Usedef.count_stmts_use usedefmap;
  writedump mach (Printf.sprintf "dump-%ld-%ld.02.deadcode.dot" fromblk toblk)
    vertices'' [`VERTEX; `PRED; `SUCC; `DOMFRONT];
  propeval vertices'' usedefmap;
  writedump mach (Printf.sprintf "dump-%ld-%ld.03.consteval.dot" fromblk toblk)
    vertices'' [`VERTEX; `PRED; `SUCC; `DOMFRONT];
(*  remove_uninteresting vertices'';
  writedump (Printf.sprintf "dump-%ld-%ld.04.uninteresting.dot" fromblk toblk)
    vertices'' [`VERTEX; `PRED; `SUCC; `DOMFRONT]; *)
  Layout.layout vertices'' mach#getserv#getindexer vroot fromblk;
  DynArray.iter
    (fun tag -> tag.Block.selected <- Selectdrv.insn_selection vertices'' tag)
    vertices'';
  let intf = Interference2.liveness_analysis vertices''
  and fixed = Precolour.find_fixed_assignments vertices'' in
  writedump mach (Printf.sprintf "dump-%ld-%ld.05.liveness.dot" fromblk
    toblk) vertices'' [`VERTEX; `PRED; `SUCC]
    (* ~extra:(write_interference intf)  *);
  let file = open_out
    (Printf.sprintf "dump-%ld-%ld.06.intf.txt" fromblk toblk) in
  write_interference intf file;
  close_out file;
  (* Precolour.write_fixed_assignments fixed; *)
  let cols, spilled = Colour.alloc fixed intf (Regpool.initial_pools)
  and file = open_out (Printf.sprintf "dump-%ld-%ld.07.regalloc.txt" fromblk
    toblk) in
  Colour.write_alloc file cols spilled intf;
  close_out file;
  Phi.eliminate vertices'';
  (* insert spill code *)
  (* rewrite pseudos & subscripted registers with hardware registers *)
  Solidify.rewrite mach vertices'' cols;
  Killmov.killmov vertices'';
  writedump mach (Printf.sprintf "dump-%ld-%ld.09.killmov.dot" fromblk toblk)
    vertices'' [`ASSEMBLY; `SUCC];
  (* encode binary *)
  Encode.showencoded (Encode.encodevertices vertices'')
  (* make block(s) *)

