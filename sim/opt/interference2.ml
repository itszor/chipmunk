(* Liveness analysis on variables used by selected instructions.

   Represent the interference graph as a map from reg_or_pseudo to sets of 
   reg_or_pseudos.
   We already have the latter as Sets.RegOrPseudoSet, and the former as
   Sets.RegOrPseudoMap, so just use them both together.
*)

module S = Sets.RegOrPseudoSet
module M = Sets.RegOrPseudoMap
module I = Sets.IntSet
module IntfGraph = Sets.IntfGraph

type intfmap = S.t M.t

module StmtSet = Set.Make (
  struct
    type t = Select.selectedast * Block.tag
    let compare = (fun (s1,t1) (s2,t2) ->
                     compare (s1.Select.s_num, t1.Block.dfnum)
                             (s2.Select.s_num, t2.Block.dfnum))
  end)

let stmt_uses_var var stmt tag map =
  (* Printf.printf "stmt_uses_var: %s\n" (Id.string_of_rop var); *)
  try
    let set = M.find var map in
    let set' = StmtSet.add (stmt, tag) set in
    M.add var set' map
  with Not_found ->
    M.add var (StmtSet.singleton (stmt, tag)) map

(*
let interfere' intf a b =
  try
    let set = M.find a intf in
    let set' = S.add b set in
    M.add a set' intf
  with Not_found ->
    M.add a (S.singleton b) intf

(* This is a two-way relationship; specify ordering to avoid needing to
   store the interference both ways.  *)
let interfere intf a b =
  if a<b then
    interfere' intf a b
  else
    interfere' intf b a
*)

let find_var_uses vertices =
  DynArray.fold_right
    (fun tag map ->
      List.fold_right
        (fun stmt map ->
          List.fold_right
            (fun var map -> stmt_uses_var var stmt tag map)
            stmt.Select.s_use
            map)
        tag.Block.selected
        map)
    vertices
    M.empty

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
  let rec scan num lis =
    match lis with
      [] -> 0, []
    | t::ss when t==stmt -> num, ss
    | t::ss -> scan (num-1) ss
  in scan (List.length tag.Block.selected) tag.Block.selected

let rec liveness_analysis vertices =
  let var_use_map = find_var_uses vertices in
  M.fold
    (fun var stmts intf ->
      (* Printf.printf "liveness_analysis: tracking var %s\n"
        (Id.string_of_rop var); *)
      let intf = IntfGraph.add_node var intf in
      let intf', _ = StmtSet.fold
        (fun (stmt, tag) (intf, visited) ->
          match stmt.Select.s_asm with
            Ast.Iphi(dst, args) ->
              (* Printf.printf "Phi node\n"; *)
              begin try
                let i = seek_arg args (Ast.ast_of_reg_or_pseudo var Ast.Use) in
                let p = List.nth tag.Block.predecessors i in
                live_out_at_block ~visited:visited ~interf:intf ~block_tag:p
                                  ~var:var
              with Not_found ->
                failwith "Predecessor not found for Phi node"
              end
          | _ ->
              (* Printf.printf "Regular stmt\n"; *)
              let num, prev = stmts_before stmt tag in
              live_in_at_statement ~visited:visited ~interf:intf ~stmt:stmt
                                   ~stmt_num:num ~preceding:prev ~block_tag:tag
                                   ~var:var)
        stmts
        (intf, I.empty)
      in
        intf')
    var_use_map
    IntfGraph.empty

and live_out_at_block ~visited:m ~interf:i ~block_tag:n ~var:v =
 (* Printf.printf "live_out_at_block: %s\n" (Id.string_of_rop v); *)
  n.Block.live_out <- S.add v n.Block.live_out;
  if not (I.mem n.Block.dfnum m) then begin
    let visited = I.add n.Block.dfnum m in
    match n.Block.selected with
      [] -> (* i, visited *)
        Printf.printf "Faking dummy stmt!\n";
        (* The block doesn't have any statements, but we must not stop
           processing yet. Pass a dummy statement. This seems wrong.  *)
        let dummy = { Select.s_asm = Ast.Insn (fun _ -> []);
                      s_def = [];
                      s_use = [];
                      s_live_in = Sets.RegOrPseudoSet.empty;
                      s_live_out = Sets.RegOrPseudoSet.empty;
                      s_num = -1 }
        in
          live_out_at_statement ~visited:visited ~interf:i ~stmt:dummy
            ~stmt_num:0 ~preceding:[] ~block_tag:n ~var:v
    | last_stmt::previous_stmts ->
        live_out_at_statement ~visited:visited ~interf:i ~stmt:last_stmt
                              ~stmt_num:(List.length previous_stmts)
                              ~preceding:previous_stmts ~block_tag:n ~var:v
  end else begin
    (* Printf.printf "Seen already!\n"; *)
    i, m
  end

and live_in_at_statement ~visited:m ~interf:i ~stmt:s ~stmt_num:num
                         ~preceding:pstmt ~block_tag:tag ~var:v =
(*  Printf.printf "live_in_at_statement: %s\n" (Id.string_of_rop v); *)
  s.Select.s_live_in <- S.add v s.Select.s_live_in;
  match pstmt with
    [] ->
      tag.Block.live_in <- S.add v tag.Block.live_in;
      List.fold_right
        (fun p (i,m) -> live_out_at_block ~visited:m ~interf:i ~block_tag:p
                                          ~var:v)
        tag.Block.predecessors
        (i,m)
  | previous::remaining ->
      live_out_at_statement ~visited:m ~interf:i ~stmt:previous
                            ~stmt_num:(num-1) ~preceding:remaining
                            ~block_tag:tag ~var:v

and live_out_at_statement ~visited:m ~interf:i ~stmt:s ~stmt_num:num
                          ~preceding:pstmts ~block_tag:tag ~var:v =
(*  Printf.printf "live_out_at_statement: %s\n" (Id.string_of_rop v); *)
  s.Select.s_live_out <- S.add v s.Select.s_live_out;
  let i = List.fold_right
    (fun w i -> if w<>v then begin
      (*Printf.printf "live_out_at_statement: interfering %s and %s\n"
        (Id.string_of_rop w) (Id.string_of_rop v); *)
        IntfGraph.add w v i; end else i)
    s.Select.s_def
    i
  in
    if not (List.mem v s.Select.s_def) then
      live_in_at_statement ~visited:m ~interf:i ~stmt:s ~stmt_num:num
                           ~preceding:pstmts ~block_tag:tag ~var:v
    else
      i, m
