(* Calculate interference graph for registers & pseudos

   Represent the interference graph as a map from reg_or_pseudo to sets of 
   reg_or_pseudos.
   We already have the latter as Sets.RegOrPseudoSet, and the former as
   Sets.RegOrPseudoMap, so just use them both together.
*)

open Sets
open Block

(* Just an alias to make life easier *)
type intfmap = Sets.RegOrPseudoSet.t Sets.RegOrPseudoMap.t

let seek_arg args v =
  let total = Array.length args in
  let rec scan i =
    if i=total then
      raise Not_found
    else
      if v=args.(i) then i else scan (i+1)
  in
    scan 0

(* Gives list of stmts before a particular stmt in a block.
   (and the number of the stmt)
 *)
let stmts_before stmt tag =
  let Block(astlist,_) = tag.block in
  let rec scan num lis =
    match lis with
      [] -> 0, []
    | t::ss when t==stmt -> num, ss
    | t::ss -> scan (num-1) ss
  in scan (List.length astlist) astlist

let vars_defined_by stmt =
  Ast.fold_ltor_postorder
    (fun ast acc ->
      match ast with
        Ast.Register(id, typ, Ast.Assign)
      | Ast.Register(id, typ, Ast.FixedAssign) -> (Id.PhysReg(id,typ))::acc
      | Ast.Pseudo(ps, typ, Ast.Assign)
      | Ast.Pseudo(ps, typ, Ast.FixedAssign) -> (Id.PseudoReg(ps,typ))::acc
      | _ -> acc)
    stmt
    []

let interfere intf a b =
  try
    let set = RegOrPseudoMap.find a intf in
    let set' = RegOrPseudoSet.add b set in
    RegOrPseudoMap.add a set' intf
  with Not_found ->
    RegOrPseudoMap.add a (RegOrPseudoSet.singleton b) intf

(* The interference is only calculated "one way" and the relation is
 * symmetric, so check both ways.
 *)
let interferes intf a b =
  let check a' b' =
    try
      let set = RegOrPseudoMap.find a' intf in
      RegOrPseudoSet.mem b' set
    with Not_found -> false
  in
    check a b || check b a

(* Algorithm straight from Appel's book. *)

let rec liveness_analysis vars =
  let intf = ref RegOrPseudoMap.empty in
  Sets.RegOrPseudoMap.iter
    (fun v usedef ->
      List.iter
        (fun (stmt,tag) ->
          let m = ref Sets.IntSet.empty in
          match stmt with
            Ast.Move(_, Ast.Phi args) ->
              begin try
                let i = seek_arg args (Ast.ast_of_reg_or_pseudo v Ast.Use) in
                let p = List.nth tag.predecessors i in
                live_out_at_block ~visited:m ~interf:intf ~block_tag:p ~var:v
              with Not_found ->
                let num, prev = stmts_before stmt tag in
                live_in_at_statement ~visited:m ~interf:intf ~stmt:stmt 
                                     ~stmt_num:num ~preceding:prev 
                                     ~block_tag:tag ~var:v
              end
          | _ ->
            let num, prev = stmts_before stmt tag in
            live_in_at_statement ~visited:m ~interf:intf ~stmt:stmt
                                 ~stmt_num:num ~preceding:prev
                                 ~block_tag:tag ~var:v)
        usedef.Usedef.stmts_use)
    vars;
  !intf

and live_out_at_block ~visited:m ~interf:i ~block_tag:n ~var:v =
 (* Printf.printf "%s is live-out at block %d\n"
    (Id.string_of_rop v) n.Block.dfnum; *)
  n.live_out <- Sets.RegOrPseudoSet.add v n.live_out;
  if not (Sets.IntSet.mem n.dfnum !m) then begin
    m := Sets.IntSet.add n.dfnum !m;
    let Block(astlist,_) = n.block in
    match astlist with
      [] -> ()
    | last_stmt::previous_stmts ->
      live_out_at_statement ~visited:m ~interf:i ~stmt:last_stmt
                            ~stmt_num:(List.length previous_stmts)
                            ~preceding:previous_stmts ~block_tag:n ~var:v
  end

and live_in_at_statement ~visited:m ~interf:i ~stmt:s ~stmt_num:num
                         ~preceding:pstmt ~block_tag:tag ~var:v =
 (* Printf.printf "%s is live-in at stmt %s\n"
    (Id.string_of_rop v) (Disast.writeop s); *)
  let Block(astlist,_) = tag.block in
  match pstmt with
    [] ->
     (* Printf.printf "%s is live-in at block %d\n"
        (Id.string_of_rop v) (tag.Block.dfnum); *)
      tag.live_in <- Sets.RegOrPseudoSet.add v tag.live_in;
      List.iter
        (fun p -> live_out_at_block ~visited:m ~interf:i ~block_tag:p ~var:v) 
        tag.predecessors
  | previous::remaining ->
    live_out_at_statement ~visited:m ~interf:i ~stmt:previous 
                          ~stmt_num:(num-1) ~preceding:remaining ~block_tag:tag
                          ~var:v

and live_out_at_statement ~visited:m ~interf:i ~stmt:s ~stmt_num:num
                          ~preceding:pstmts ~block_tag:tag ~var:v =
(*  Printf.printf "%s is live-out at stmt %s\n"
    (Id.string_of_rop v) (Disast.writeop s); *)
  let wset = vars_defined_by s in
  List.iter
    (fun w ->
      if w<>v then begin
        i := interfere !i w v
      (*  Printf.printf "(%s,%s) added to interference graph\n"
          (Id.string_of_rop v)
          (Id.string_of_rop w)*)
      end)
    wset;
  if not (List.mem v wset) then
    live_in_at_statement ~visited:m ~interf:i ~stmt:s ~stmt_num:num
                         ~preceding:pstmts ~block_tag:tag ~var:v
      
