(* Try to find a working colouring for registers & pseudos *)

module S = Sets.RegOrPseudoSet
module M = Sets.RegOrPseudoMap
module IntfGraph = Sets.IntfGraph

type place = Spill | Allocate

(* Find a set of reg-or-pseudos which is already allocated by the neighbours
   of a node.  *)
let neighbours_use nlist colouring =
  List.fold_right
    (fun node acc ->
      try
        let found = M.find node colouring in
        Regpool.mark_allocated found acc
      with Not_found -> acc)
    nlist
    S.empty

let alloc fixassign intf regpool =
  let min_degree graph =
    IntfGraph.fold_node
      (fun node edges lo ->
       (* Printf.printf "Alloc: node %s\n" (Id.string_of_rop node); *)
        match lo with
          None -> Some (node, IntfGraph.degree_i edges)
        | Some (_,lo_deg) ->
            let new_deg = IntfGraph.degree_i edges in
            if new_deg < lo_deg then Some (node, new_deg) else lo)
      graph
      None
  in let rec remove_nodes intf' colouring spilled =
    match min_degree intf' with
      None -> colouring, spilled
    | Some (node,md) ->
      (*  Printf.printf "Colour: node %s, degree %d\n"
          (Id.string_of_rop node) md; *)
        let without_node = IntfGraph.remove_node node intf' in
        let cols,spilled' =
          remove_nodes without_node colouring spilled
        in
          (* The node's neighbours when it is put back in.  *)
          let neighbours = IntfGraph.connected node intf'
          (* All the fixed-assign neighbours.  *)
          and all_neighbours = IntfGraph.connected node intf in
          let n_use = neighbours_use neighbours cols
          and n_fix = neighbours_use all_neighbours fixassign in
          try
            let chosen =
              Regpool.choose_reg fixassign n_use n_fix node regpool
            in
              M.add node chosen cols, spilled'
          with
            Regpool.Spill -> cols, node :: spilled'
  in
    remove_nodes intf M.empty []

let write_regmap file map =
  M.iter
    (fun a b ->
      Printf.fprintf file "%s -> %s\n" (Id.string_of_rop a)
        (Id.string_of_rop b))
    map

let write_alloc file cols spilled intf =
  Printf.fprintf file "Spilled:\n";
  List.iter
    (fun spill -> Printf.fprintf file "%s (intf %s)\n" (Id.string_of_rop spill)
      (String.concat "," (List.map Id.string_of_rop
                                   (IntfGraph.connected spill intf))))
    spilled;
  Printf.fprintf file "Allocated:\n";
  write_regmap file cols
