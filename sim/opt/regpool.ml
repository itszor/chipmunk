(* Register pools for graph colouring. Int and ptr reg pools overlap;
   handle that here.  *)

module S = Sets.RegOrPseudoSet

module M = Sets.RegOrPseudoMap

let from_to = Common.from_to

type t = {
  intregs : S.t;
  ptrregs : S.t;
  floatregs : S.t
}

let inttype_pool =
  (* FIXME: The list of int regs doesn't include r55, link register. All
     others are general purpose.  *)
  let also_ptrs =
    from_to (fun n a -> S.add (Id.PhysReg((n, Id.Unset), Id.IntType)) a)
          S.empty 56 63
  in
    from_to (fun n a -> S.add (Id.PhysReg((n, Id.Unset), Id.IntType)) a)
            also_ptrs 0 54

let ptrtype_pool =
  from_to (fun n a -> S.add (Id.PhysReg((n, Id.Unset), Id.PtrType)) a)
          S.empty 56 63

let floattype_pool =
  from_to (fun n a -> S.add (Id.PhysReg((n, Id.Unset), Id.FloatType)) a)
          S.empty 0 63

let initial_pools = {
  intregs = inttype_pool;
  ptrregs = ptrtype_pool;
  floatregs = floattype_pool
}

let choose_pool pools = function
    Id.PhysReg(_, idtype)
  | Id.PseudoReg(_, idtype) ->
      begin match idtype with
        Id.IntType -> pools.intregs
      | Id.PtrType -> pools.ptrregs
      | Id.FloatType -> pools.floatregs
      end

let mark_allocated reg in_list =
  match reg with
    Id.PhysReg((n, Id.Unset) as id, Id.IntType) ->
      if n >= 56 && n < 64 then
        let ptr_of_int = Id.PhysReg(id, Id.PtrType) in
        S.add reg (S.add ptr_of_int in_list)
      else
        S.add reg in_list
  | Id.PhysReg((n, Id.Unset) as id, Id.PtrType) ->
      if n >= 56 && n < 64 then
        let int_of_ptr = Id.PhysReg(id, Id.IntType) in
        S.add reg (S.add int_of_ptr in_list)
      else
        S.add reg in_list
  | Id.PhysReg((n, Id.Unset) as id, Id.FloatType) ->
      S.add reg in_list
  | _ -> failwith "Non-physical register allocated."

exception Spill

(* Choose a register which isn't already taken (i.e. in "not_from"). The
   type of the register should be (pool_sel : pool_selector). Try to allocate
   first from "fixed", and spill immediately if that fails.
   Return: the register, or throw "Spilled" exception.  *)
let choose_reg fixed notfrom fixedconflict node pools =
  let pool = choose_pool pools node in
  let unused = S.diff pool notfrom in
  try
    let fixed_alloc = M.find node fixed in
    if S.mem fixed_alloc unused then
      fixed_alloc
    else
      (* We tried to do a fixed allocation of a register which isn't
         available. Spill it instead.  *)
      raise Spill
  with Not_found ->
    (* Don't use a register which conflicts with a neighbouring fixed-use.  *)
    let nofix = S.diff unused fixedconflict in
    (* Choose a register from the pool, or spill.  *)
    if S.cardinal nofix > 0 then
      S.choose nofix
    else
      raise Spill
