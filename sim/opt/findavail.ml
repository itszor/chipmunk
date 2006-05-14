(* Find available register sets (otherwise dead ranges) for everywhere in
   a memoized AST list
*)

open Ast
open Sets

type reg_note = DefinitionKilled of Id.id
              | UsageFolded of Id.id
              | Defined of Id.id
              | Used of Id.id

let (++) a b = b a

(* Return an IntSet of register numbers defined by this AST node (ie move) *)
(*let definedregs note =
  let scan el ids =
    match el with
      Defined(num,loc) -> IntSet.add num ids
    | _ -> ids
  in
    RegnoteSet.fold scan note IntSet.empty*)

(*
let findregs masts =
  let rec scan m lastset =
    m.ireginfo.avail <- lastset;
    match m.node with
      Triop(op,a,b,c) ->
        scan c lastset ++ scan b ++ scan a
    | Binop(op,a,b) ->
        scan b lastset ++ scan a
    | Unop(op,a) ->
        scan a lastset
    | Nop(op) -> lastset
    | Move(a,b) ->
       let bset = scan b lastset in
       let def = definedregs m.ireginfo.note in
       if IntSet.is_empty def then
         scan a bset
       else
         IntSet.union bset def
    | Constant(c) -> lastset
    | Floatconst(c) -> lastset
    | Pair(a,b) ->
        scan b lastset ++ scan a
    | Register((num,loc),expi) ->
        let newset =
          match expi with
            Iformat.Dead -> IntSet.remove num lastset
          | _ -> lastset
        in
          lastset
    | Floatreg((num,loc),expi) -> lastset
    | _ -> lastset
  in let rec scanlist m lastset =
    match m with
      [] -> lastset
    | m::ms -> let next = scan m lastset in scanlist ms next
  in
    scanlist (List.rev masts) IntSet.empty
*)
