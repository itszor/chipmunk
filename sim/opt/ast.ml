(* An abstract syntax tree form for decompiled code *)

type ast = Triop of op3 * ast * ast * ast
         | Binop of op2 * ast * ast
         | Unop of op1 * ast
         | Zop of op0
         | Move of ast * ast
         | Register of reg
         | Pseudo of pseudo
         | Constant of int32
         | Floatconst of float
         | Treg of ldtype
         | Note of note * ast
         | Phi of ast array
         | Syncregs
         | Reduction of red
(*         | CstReduct of cst * red *)
         | Null

and red = {
  reduction : ast;
  ruleno : int;
  cost : int;
  cost_with_kids : int;
  defs : ast list;
  uses : ast list;
  factored_vars : ast list;
  insn : insnorphi
}

and insnorphi =
    Insn of (Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Iformat.format list)
  | Ainsn of Iformat.format list
  | Iphi of ast * ast array

(* Extra info for constants in case we make a wrong decision and have to 
   backtrack.  *)
(* and cst = {
  constval : int32;
  backtrack : ast
} *)

(*and reginfo = {
  mutable note : RegnoteSet.t;
  mutable avail : IntSet.t
}*)

and reg = Id.id * Id.regtype * usage

and pseudo = int * Id.regtype * usage

and usage = Assign | Use | LastUse | FixedAssign

and size = Byte | Halfword | Word | Doubleword | WordVec | DoublewordVec

and ldtype = X | Y | Z

and op0 = Return

and op1 = Not
        | Ind of size
        | IndVol of size
        | Swi
        | Jump
        | Neg_f of Iformat.precision
        | Abs_f of Iformat.precision
        | Sqr_f of Iformat.precision
        | Cast_f of Iformat.precision

and op2 = Add
        | Sub
        | Mul
        | Div
        | Lsl
        | Lsr
        | Asr
        | Ror
        | And
        | Ior
        | Eor
        | Cmp of Iformat.cond
        | Udiv
        | Mod
        | Umod
        | Mlh
        | Umlh
        | Call
        | Add_f of Iformat.precision
        | Sub_f of Iformat.precision
        | Mul_f of Iformat.precision
        | Div_f of Iformat.precision
        | Trap of Iformat.tcond

and op3 = Branch
        | Bitfield
        | SignedBitfield
        | SpliceShift
        | Splice

and note = ExpireHere of regset

and regset = {
  mutable exp_ints : Sets.IntSet.t;
  mutable exp_flts : Sets.IntSet.t
}

module AstSet = Set.Make (
  struct
    type t = ast
    let compare = compare
  end)

module AstMap = Map.Make (
  struct
    type t = ast
    let compare = compare
  end)

exception AstNotRegisterLike of ast

let ast_of_reg_or_pseudo r_or_p usage =
  match r_or_p with
    Id.PhysReg(id, typ) -> Register(id, typ, usage)
  | _ -> failwith "Unknown reg or pseudo"

let rec reg_or_pseudo_of_ast = function
    Register(id, typ, _) -> Id.PhysReg(id,typ)
  | Pseudo(ps, typ, _) -> Id.PseudoReg(ps,typ)
  | Reduction {reduction = theast} -> reg_or_pseudo_of_ast theast
(*  | m ->
    Printf.printf "getting pseudo from: %s\n" (Disast.writeop m);
    Id.PhysReg((22, Id.Unset), Id.IntType) *)
  | x -> raise (AstNotRegisterLike x)

let is_register = function
    Register _ -> true
  | Pseudo _ -> true
  | _ -> false

let iter fn ast =
  let rec scan ast =
    fn ast;
    match ast with
      Triop(_, a, b, c) -> scan a; scan b; scan c
    | Binop(_, a, b) -> scan a; scan b
    | Unop(_, a) -> scan a
    | Move(a,b) -> scan a; scan b
    | Note(_,a) -> scan a
    | Phi(ar) -> Array.iter scan ar
    | _ -> ()
  in
    scan ast

(* Do postorder, right-to-left traversal of AST tree *)
let fold_rtol_postorder fn ast acc =
  let rec scan ast acc =
    match ast with
      Triop(_,a,b,c) ->
        let c' = scan c acc in
        let b' = scan b c' in
        let a' = scan a b' in
        fn ast a'
    | Binop(_,a,b) ->
        let b' = scan b acc in
        let a' = scan a b' in
        fn ast a'
    | Unop(_,a) ->
        let a' = scan a acc in
        fn ast a'
    | Move(a,b) ->
        let b' = scan b acc in
        let a' = scan a b' in
        fn ast a'
    | Note(n,a) ->
        let a' = scan a acc in
        fn ast a'
    | Phi(ar) ->
        let ar' = Array.fold_right scan ar acc in
        fn ast ar'
    | x -> fn x acc
  in
    scan ast acc

(* Do postorder, left-to-right traversal of AST tree *)
let fold_ltor_postorder fn ast acc =
  let rec scan ast acc =
    match ast with
      Triop(_,a,b,c) ->
        let a' = scan a acc in
        let b' = scan b a' in
        let c' = scan c b' in
        fn ast c'
    | Binop(_,a,b) ->
        let a' = scan a acc in
        let b' = scan b a' in
        fn ast b'
    | Unop(_,a) ->
        let a' = scan a acc in
        fn ast a'
    | Move(a,b) ->
        let a' = scan a acc in
        let b' = scan b a' in
        fn ast b'
    | Note(n,a) ->
        let a' = scan a acc in
        fn ast a'
    | Phi(ar) ->
        let ar' = Array.fold_right scan ar acc in
        fn ast ar'
    | x -> fn x acc
  in
    scan ast acc

(* Some helper functions to fold over AST lists *)
let list_fold_forwards fn acc astlist =
  let fn' a b = fn b a in
  List.fold_right (fun ast acc -> fold_ltor_postorder fn' ast acc) astlist acc

let list_fold_backwards fn astlist acc =
  List.fold_left (fun acc ast -> fold_rtol_postorder fn ast acc) acc astlist
 
let map_postorder fn ast =
  let rec scan = function
    Triop(op,a,b,c) ->
      let a' = scan a
      and b' = scan b
      and c' = scan c in
      fn (Triop(op,a',b',c'))
  | Binop(op,a,b) ->
      let a' = scan a
      and b' = scan b in
      fn (Binop(op,a',b'))
  | Unop(op,a) ->
      let a' = scan a in
      fn (Unop(op,a'))
  | Move(a,b) ->
      let a' = scan a
      and b' = scan b in
      fn (Move(a',b'))
  | Note(n,a) ->
      let a' = scan a in
      fn (Note(n,a'))
  | Phi(ar) ->
      let ar' = Array.map scan ar in
      fn (Phi(ar'))
  | x -> fn x
  in
    scan ast

(* This function returns the sequence in reverse order, beware *)
let seq_of_ast_rl ast =
  fold_rtol_postorder (fun a b -> Seq.cons a b) ast Seq.Empty

(* This function also returns the sequence in reverse order *)
let seq_of_ast_lr ast =
  fold_ltor_postorder (fun a b -> Seq.cons a b) ast Seq.Empty

(* Dodgy-as-fuck transformation. We have prefix-notation sequence defining
 * our tree. So we build a new tree, using "leaf" nodes as
 * the new nodes, and treating non-leaf nodes purely as structural things,
 * ignoring their children (but using their "op" field).
 * "Leaf" nodes are: Constant, Register, {Int,Float}pseudo, Floatreg, Treg,
 * MatchDomain
 * "Nonleaf" nodes are: Triop, Binop, Unop, Move, Note(?), Phi
 *)
let ast_of_seq_rl seq =
  let rec scan seq =
    match seq with
      Seq.Empty -> failwith "Sequence empty"
    | Seq.Cons(hd,tl) ->
        begin match hd with
          Register _
        | Pseudo _
        | Constant _ 
        | Floatconst _
        | Syncregs
        | Treg _
        | Zop _
        | Null -> hd, Lazy.force tl
        | Triop(op,_,_,_) ->
            let a,n1 = scan (Lazy.force tl) in
            let b,n2 = scan n1 in
            let c,n3 = scan n2 in
            Triop(op,a,b,c), n3
        | Binop(op,_,_) ->
            let a,n1 = scan (Lazy.force tl) in
            let b,n2 = scan n1 in
            Binop(op,a,b), n2
        | Unop(op,_) ->
            let a,n1 = scan (Lazy.force tl) in
            Unop(op,a), n1
        | Move(_,_) ->
            let a,n1 = scan (Lazy.force tl) in
            let b,n2 = scan n1 in
            Move(a,b), n2
        | Note(n,_) ->
            let a,n1 = scan (Lazy.force tl) in
            Note(n,a), n1
        | Phi(ar) ->
            failwith "Phi not done here"
        end
  in
    let out,_ = scan seq in out

(* Teh copy & paste *)
let ast_of_seq_lr seq =
  let rec scan seq =
    match seq with
      Seq.Empty -> failwith "Sequence empty"
    | Seq.Cons(hd,tl) ->
        begin match hd with
          Register _
        | Pseudo _
        | Constant _ 
        | Floatconst _
        | Zop _
        | Null
        | Syncregs
        | Treg _ -> hd, Lazy.force tl
        | Triop(op,_,_,_) ->
            let a,n1 = scan (Lazy.force tl) in
            let b,n2 = scan n1 in
            let c,n3 = scan n2 in
            Triop(op,c,b,a), n3
        | Binop(op,_,_) ->
            let a,n1 = scan (Lazy.force tl) in
            let b,n2 = scan n1 in
            Binop(op,b,a), n2
        | Unop(op,_) ->
            let a,n1 = scan (Lazy.force tl) in
            Unop(op,a), n1
        | Move(_,_) ->
            let a,n1 = scan (Lazy.force tl) in
            let b,n2 = scan n1 in
            Move(b,a), n2
        | Note(n,_) ->
            let a,n1 = scan (Lazy.force tl) in
            Note(n,a), n1
        | Phi(ar) ->
            failwith "Phi not done here"
        end
  in
    let out,_ = scan seq in out
