(* Simplify a fragment by mushing together branches to branches.
 *
 * DFS should have been run before this.
 *
 * The only things expected to be valid in block tags are:
 *   block: the AST block
 *   refno: A unique ID (i.e. the address for non-generated blocks)
 *   successor list
 *   predecessor list
 *
 * Calculation proceeds in reverse depth-first search order, so that later
 * (higher-numbered) blocks can be merged with earlier blocks without needing
 * multiple passes.
 *)

open Block

let is_empty tag =
  let Block(astlist, term) = tag.block in
  match astlist with
    [] -> true
  | _ -> false

let rec unsetdfs fragment =
  let unvisited blkref =
    match blkref with
      Local(dest) -> if dest.dfnum != -1 then unsetdfs dest
    | _ -> ()
  in
  fragment.dfnum <- -1;
  let Block(_, term) = fragment.block in
  match term with
    CondBranch(_, _, trueblk, falseblk) ->
      unvisited trueblk;
      unvisited falseblk
  | Call(callblk, retblk) ->
      unvisited callblk;
      unvisited retblk
  | Jump(toblk) ->
      unvisited toblk
  | Return -> ()

let undodfs vertices =
  for i=0 to (DynArray.length vertices)-1 do
    let tag = DynArray.get vertices i in
    tag.dfnum <- -1;
    tag.predecessors <- [];
    tag.successors <- [];
  done

let collapsible fromblk toblk =
  match fromblk.successors, toblk.predecessors with
    [s], [p] -> s==toblk && p==fromblk
  | _ -> false

let simplify vertices =
  let num = DynArray.length vertices in
  for i=num-1 downto 0 do
    let fragment = DynArray.get vertices i in
    let Block(astlist, term) = fragment.block in
    match term with
      Jump(toblk) ->
        begin match toblk with
          Local(dest) ->
            if collapsible fragment dest then begin
              let Block(astlist_d, term_d) = dest.block in
              let blk' = Block(astlist_d @ astlist, term_d) in
              fragment.block <- blk';
              Printf.printf "Joined blocks %d and %d\n"
                fragment.dfnum dest.dfnum
            end else if is_empty fragment then begin
              Printf.printf "Eliminated empty block %d\n" fragment.dfnum;
              fragment.block <- dest.block
            end
        | _ -> ()
        end
    | _ -> ()
  done;
  undodfs vertices;
  DynArray.get vertices 0






