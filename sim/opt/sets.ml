(* Set & map modules.  *)

(*module RegnoteSet = Set.Make (
  struct
    type t = reg_note
    let compare = compare
  end)
;;
*)

module IdSet = Set.Make (
  struct
    type t = Id.id
    let compare = compare
  end)

module IntSet = Set.Make (
  struct
    type t = int
    let compare = compare
  end)

module RefSet = Set.Make (
  struct
    type t = int32
    let compare = compare
  end)

module RegOrPseudoSet = Set.Make (
  struct
    type t = Id.reg_or_pseudo
    let compare = compare
  end)

module SpanMap = Map.Make (
  struct
    type t = Id.reg_or_pseudo
  (*  type t = { born : int; dies : int; broken : bool }*)
    let compare = compare
  end)
  
module RegOrPseudoMap = Map.Make (
  struct
    type t = Id.reg_or_pseudo
    let compare = compare
  end)

module IntfGraph = Graph.Make (
  struct
    type t = Id.reg_or_pseudo
    let compare = compare
  end)
