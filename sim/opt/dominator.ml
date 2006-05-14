(* Should find the dominator tree for a code fragment using the algorithm of
 * Lengauer and Tarjan.
 *
 * Does not yet implement the "path compression" optimisation, though it would
 * be very easy to do...
 *)

open Block

(* Teh proper dfs alg0ritmh
 * fills in predecessor & successor lists
 *)
let dfs blktag =
  let num = ref 0
  and vertex = DynArray.create () in
  let rec scan parent node =
    begin match parent with
      Some x -> node.predecessors <- x :: node.predecessors
    | None -> ()
    end;
    if node.dfnum = -1 then begin
      node.dfnum <- !num;
      DynArray.add vertex node;
      node.parent <- parent;
      incr num;
      let Block(_,term) = node.block in
      match term with
        Jump (Local child) ->
          node.successors <- child :: node.successors;
          scan (Some node) child
      | Call (Local child, _) ->
          node.successors <- child :: node.successors;
          scan (Some node) child
      | CondBranch (_, _, Local a, Local b) ->
          node.successors <- a :: b :: node.successors;
          scan (Some node) a;
          scan (Some node) b
      | CondBranch (_, _, Local a, _) ->
          node.successors <- a :: node.successors;
          scan (Some node) a
      | CondBranch (_, _, _, Local b) ->
          node.successors <- b :: node.successors;
          scan (Some node) b
      | CondBranch _ -> ()
      | _ -> ()
    end
  in
    scan None blktag;
    vertex

let ancestorWithLowestSemi vv =
  let rec scan u v =
    match v.ancestor with
      None -> u
    | Some v_anc ->
        if (Option.get v.semi).dfnum < (Option.get u.semi).dfnum then
          scan v v_anc
        else
          scan u v_anc
  in
    scan vv vv

(*  let u = ref vv and v = ref vv in
  while (match !v.ancestor with None -> false | _ -> true) do
    if (Option.get !v.semi).dfnum < (Option.get !u.semi).dfnum then u := !v;
    v := Option.get !v.ancestor
  done;
  !u*)

let link p n =
 (* Printf.printf "putting ancestor of dfs-block %d to dfs-block %d\n"
    n.dfnum (Option.get p).dfnum;*)
  n.ancestor <- p

(* Finds children of immediate-dominator tree (idomchild) *)
let rec invertidom vertices num =
  for i = 1 to num-1 do
    let node = DynArray.get vertices i in
    let idom = Option.get node.idom in
    idom.idomchild <- node :: idom.idomchild
  done

let dominators vertex =
  let num = DynArray.length vertex in
  for i = num-1 downto 1 do
    let n = DynArray.get vertex i in
    let p = n.parent in
    let s = ref (Option.get p) in
    List.iter (fun v ->
                let s' = if v.dfnum <= n.dfnum then v
                         else (Option.get (ancestorWithLowestSemi v).semi)
                in if s'.dfnum < !s.dfnum then s := s')
              n.predecessors;
    n.semi <- Some !s;
    !s.bucket <- n :: !s.bucket;
    link p n;
    List.iter (fun v ->
                let y = ancestorWithLowestSemi v in
                if (Option.get y.semi) == (Option.get v.semi) then
                  v.idom <- p
                else
                  v.samedom <- Some y)
              (Option.get p).bucket;
    (Option.get p).bucket <- []
  done;
  for i = 1 to num-1 do
    let n = DynArray.get vertex i in
    match n.samedom with
      None -> ()
    | Some sd -> n.idom <- sd.idom  
  done;
  invertidom vertex num;
  vertex

module TagSet = Set.Make (
  struct
    type t = tag
    let compare = (fun a b -> compare a.dfnum b.dfnum)
  end)
;;

let rec dominates a b =
  match a, b.idom with
    x, Some y when x==y -> true
  | x, Some y -> dominates x y
  | x, None -> false

(*let rec dominates a b =
  match a, b.idom with
    x, Some y when x==y -> true
  | _ -> false*)

let rec computedf node =
  let df_local =
    List.fold_right
      (fun y s ->
        try
          if (Option.get y.idom) != node then
            TagSet.add y s
          else s
        with Option.No_value -> TagSet.add y s)
      node.successors
      TagSet.empty
  in
  let df_up = List.fold_right
    (fun c s ->
      computedf c;
      List.fold_right
        (fun w s ->
          if not (dominates node w) then
            TagSet.add w s
          else s)
        c.domfront
        s)
    node.idomchild
    df_local
  in
  node.domfront <- TagSet.elements df_up


