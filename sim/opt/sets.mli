(*open Id*)

module IntSet : Set.S with type elt = int

module RefSet : Set.S with type elt = int32

module IdSet : Set.S with type elt = Id.id

module RegOrPseudoSet : Set.S with type elt = Id.reg_or_pseudo

module SpanMap : Map.S with type key = Id.reg_or_pseudo

module RegOrPseudoMap : Map.S with type key = Id.reg_or_pseudo

module IntfGraph : Graph.S with type node = Id.reg_or_pseudo

(* module RegnoteSet : Set.S with type elt = reg_note *)
