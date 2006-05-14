module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type S =
  sig
    type node
    (* The type of graph nodes.  *)
    type t
    type edges
    (* The type of graphs.  *)
    val empty : t
    val is_empty : t -> bool
    val add : node -> node -> t -> t
    val add_node : node -> t -> t
    val remove : node -> node -> t -> t
    val has_edge : node -> node -> t -> bool
    val degree : node -> t -> int
    val remove_node : node -> t -> t
    val connected : node -> t -> node list
    val get_edges : node -> t -> edges
    val degree_i : edges -> int
    val connected_i : edges -> node list
    val fold_node : (node -> edges -> 'a -> 'a) -> t -> 'a -> 'a
  end

module Make (Ord : OrderedType) : S with type node = Ord.t
