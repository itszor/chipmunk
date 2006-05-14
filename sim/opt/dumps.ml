(* Output dumps *)

open Block

let blockstring blk dfs =
  let insns insn =
    List.fold_right
      (fun n a ->
        Buffer.add_string a (Disast.writeop n ^ "\\l"); a)
      insn
      (let b = Buffer.create 5 in
      Buffer.add_string b ("dfs: " ^ (string_of_int dfs) ^ "\\l");
      b)
  in
    let Block(insn, term) = blk in
    let insnbuf = insns insn in
    Buffer.add_string insnbuf (Disast.writectrl term ^ "\\l");
    Buffer.contents insnbuf

let selstring sel dfs =
  let insns insn =
    List.fold_right
      (fun n a ->
        let live_in = String.concat "," (List.map Id.string_of_rop
                        (Sets.RegOrPseudoSet.elements n.Select.s_live_in))
        and live_out = String.concat "," (List.map Id.string_of_rop
                        (Sets.RegOrPseudoSet.elements n.Select.s_live_out)) in
        Buffer.add_string a (Printf.sprintf "(%s) %d (%s)\\l"
                             live_in n.Select.s_num live_out); a)
      insn
      (let b = Buffer.create 5 in
      Buffer.add_string b ("dfs: " ^ (string_of_int dfs) ^ "\\l");
      b)
  in
    let selbuf = insns sel in
    Buffer.contents selbuf

let writev file tag =
  Printf.fprintf file "\"%lx\" [label=\"%s\" shape=\"box\"]\n"
    tag.refno (blockstring tag.block tag.dfnum)
  (*match tag.idom with
    None -> ()
  | Some dom -> Printf.printf "\"%lx\" -> \"%lx\"\n" key dom.refno*)

let selasm state insns =
  match insns with
    Ast.Insn _ -> "<unallocated insns>"
  | Ast.Iphi _ -> "<phi>"
  | Ast.Ainsn insns ->
      String.concat "" (List.map (fun x -> Disassemble.diss state x ^ "\\l")
                                 insns)

let writeasm state file tag =
  Printf.fprintf file "\"%lx\" [label=\"%s\" shape=\"box\"]\n"
    tag.refno (String.concat "" (List.rev_map
      (fun sel -> selasm state sel.Select.s_asm) tag.selected))

let writepred file tag =
  ignore
    (List.fold_left
      (fun num x ->
        Printf.fprintf file
          "\"%lx\" -> \"%lx\" [color=\"magenta\" label=\"%d\"]\n"
          tag.refno x.refno num;
          num+1)
      0
      tag.predecessors)

let writesucc file tag =
  List.iter
    (fun x -> Printf.fprintf file "\"%lx\" -> \"%lx\" [color=\"red\"]\n"
      tag.refno x.refno)
    tag.successors

let writeparent file tag =
  match tag.parent with
    None -> Printf.fprintf file "NULL -> \"%lx\" [color=\"blue\"]\n" tag.refno
  | Some p -> Printf.fprintf file "\"%lx\" -> \"%lx\" [color=\"blue\"]\n"
               p.refno tag.refno

let domfront file tag =
  List.iter
    (fun x -> Printf.fprintf file "\"%lx\" -> \"%lx\" [color=\"green\"]\n"
      tag.refno x.refno)
    tag.domfront

let kiddies file tag =
  List.iter
    (fun x -> Printf.fprintf file "\"%lx\" -> \"%lx\" [color=\"grey50\"]\n"
      tag.refno x.refno)
    tag.idomchild

let liveness file tag =
  Printf.fprintf file "\"%lx\" [label=\"%s\" shape=\"box\"]\n"
    tag.refno (selstring tag.selected tag.dfnum)

let dump state file vertex ?extra:extr features =
  let feat f =
    List.mem f features
  in
    Printf.fprintf file "digraph foo {\n";
   (* Printf.fprintf file "size=\"8.5x12\"\n"; *)
    if feat `VERTEX then DynArray.iter (writev file) vertex;
    if feat `ASSEMBLY then DynArray.iter (writeasm state file) vertex;
    if feat `LIVENESS then DynArray.iter (liveness file) vertex;
    if feat `PRED then DynArray.iter (writepred file) vertex;
    if feat `SUCC then DynArray.iter (writesucc file) vertex;
    if feat `DOMFRONT then DynArray.iter (domfront file) vertex;
   (* Hashtbl.iter kiddies vertex;*)
   (* Hashtbl.iter writeparent vertex;*)
    begin match extr with
      None -> ()
    | Some f -> f file
    end;
    Printf.fprintf file "}\n"
