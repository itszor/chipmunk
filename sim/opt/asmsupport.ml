(* Find things in environment for asm expanders *)

open Ast

let cost_int_node n =
  match n with
    Reduction { cost = cost } -> cost
  | _ -> 0

let cost_float_node n =
  match n with
    Reduction { cost = cost } -> cost
  | _ -> 0

let kids_int_node n kids =
  match n with
    Reduction { cost = cost } -> n::kids
  | _ -> kids

let kids_float_node n kids =
  match n with
    Reduction { cost = cost } -> n::kids
  | _ -> kids

let get_const = function
    Constant c -> c
  | _ -> failwith "Not a constant"

let get_num = function
    Constant c -> Int32.to_int c
  | _ -> failwith "Not a constant"

let split_lobits = function
    Constant c -> Int32.logand 0x03ffffffl c
  | _ -> failwith "Not a constant"

let split_hibits = function
    Constant c -> Int32.to_int (Int32.shift_right_logical c 26)
  | _ -> failwith "Not a constant"

let getexpire = function
    Register(_, _, LastUse)
  | Pseudo(_, _, LastUse) -> Iformat.Dead
  | _ -> Iformat.Alive

exception FailedAlloc of ast

let lookup_destreg alloc var =
  try
    let id = Ast.reg_or_pseudo_of_ast var in
    let reg = Sets.RegOrPseudoMap.find id alloc in
    match reg with
      Id.PhysReg((num, Id.Unset), (Id.IntType | Id.PtrType)) ->
        Iformat.Dreg(num)
    | _ -> failwith ("Bad reg for dest " ^ (Id.string_of_rop reg))
  with
    Ast.AstNotRegisterLike x ->
      failwith ("ast->register conversion failed for " ^ Disast.writeop x)
  | Not_found -> raise (FailedAlloc var)
     

let lookup_srcreg alloc var =
  try
    let id = Ast.reg_or_pseudo_of_ast var in
    let reg = Sets.RegOrPseudoMap.find id alloc in
    match reg with
      Id.PhysReg((num, Id.Unset), (Id.IntType | Id.PtrType)) ->
        Iformat.Reg(num, getexpire var)
    | _ -> failwith ("Bad reg for src " ^ (Id.string_of_rop reg))
  with
    Ast.AstNotRegisterLike x ->
      failwith ("ast->register conversion failed for " ^ Disast.writeop x)
  | Not_found -> raise (FailedAlloc var)
  
let lookup_destfreg alloc var =
  let id = Ast.reg_or_pseudo_of_ast var in
  let reg = Sets.RegOrPseudoMap.find id alloc in
  match reg with
    Id.PhysReg((num, Id.Unset), Id.FloatType) ->
      Iformat.Dfreg(num)
  | _ -> failwith ("Bad float reg for dest " ^ (Id.string_of_rop reg))

let lookup_srcfreg alloc var =
  let id = Ast.reg_or_pseudo_of_ast var in
  let reg = Sets.RegOrPseudoMap.find id alloc in
  match reg with
    Id.PhysReg((num, Id.Unset), Id.FloatType) ->
      Iformat.Freg(num, getexpire var)
  | _ -> failwith ("Bad float reg for src " ^ (Id.string_of_rop reg))

let pnum = ref 0

let gen_int_pseudo () =
  pnum := !pnum + 1;
  Pseudo(!pnum, Id.IntType, Use)

(*
exception NotFound of string
exception TypeMismatch of string

type ilist = Iformat.format list

and environ = (string, Ast.ast) Hashtbl.t

let usagetoexpire = function
    Ast.LastUse -> Iformat.Dead
  | Ast.Use
  | Ast.FixedAssign
  | Ast.Assign -> Iformat.Alive

let lookup_ast env var =
  try
    Hashtbl.find env var
  with Not_found -> raise (NotFound var)

let lookup_physreg alloc ast =
  let phys,usage =
    match ast with
      Ast.Register(id, typ, usage) ->
        Sets.RegOrPseudoMap.find (Id.PhysReg(id,typ)) alloc, usage
    | Ast.Pseudo(num, typ, usage) ->
        Sets.RegOrPseudoMap.find (Id.PseudoReg(num,typ)) alloc, usage
    | _ -> failwith "lookup_physreg"
  in
    match phys with
      Id.PhysReg((num, Id.Unset), typ) -> typ, usage, num
    | _ -> failwith "lookup_physreg (allocation failure)"

let lookup_destreg env alloc var =
  let foundme = lookup_ast env var in
  let typ, _, allocated = lookup_physreg alloc foundme in
  match typ with
    Id.IntType
  | Id.PtrType -> Iformat.Dreg(allocated)
  | _ -> raise (TypeMismatch var)

let lookup_destfreg env alloc var =
  let foundme = lookup_ast env var in
  let typ, _, allocated = lookup_physreg alloc foundme in
  match typ with
    Id.FloatType -> Iformat.Dfreg(allocated)
  | _ -> raise (TypeMismatch var)

let lookup_srcreg env alloc var =
  let foundme = lookup_ast env var in
  let typ, usage, allocated = lookup_physreg alloc foundme in
  match typ with
    Id.IntType
  | Id.PtrType -> Iformat.Reg(allocated, usagetoexpire usage)
  | _ -> raise (TypeMismatch var)

let lookup_srcfreg env alloc var =
  let foundme = lookup_ast env var in
  let typ, usage, allocated = lookup_physreg alloc foundme in
  match typ with
    Id.FloatType -> Iformat.Freg(allocated, usagetoexpire usage)
  | _ -> raise (TypeMismatch var)

let lookup_vol env var =
  let foundme = lookup_ast env var in
  match foundme with
    Ast.MatchVol i -> if i then Iformat.Volatile else Iformat.Nonvolatile
  | _ -> raise (TypeMismatch var)

let lookup_imm env var =
  let foundme = lookup_ast env var in
  match foundme with
    Ast.Constant(n) -> n
  | Ast.Floatconst(n) -> Int32.of_float n  (* urg, dirty *)
  | _ -> raise (TypeMismatch var)

let lookup_num env var = Int32.to_int (lookup_imm env var)

let lookup_hibits env var =
  let foundme = lookup_ast env var in
  match foundme with
    Ast.Constant(n) -> Int32.to_int (Int32.shift_right_logical n 26)
  | _ -> raise (TypeMismatch var)

let lookup_lobits env var =
  let foundme = lookup_ast env var in
  match foundme with
    Ast.Constant(n) -> Int32.logand n (Int32.of_int 0x03ffffff)
  | _ -> raise (TypeMismatch var)
*)
