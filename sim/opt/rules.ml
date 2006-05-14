(* Rules representing each DOP instruction's semantics.
   (preprocessed into rules.ml by ruleparse.cmo et al during build).
*)

open Ast
open Mtree
open Asmsupport

let rules least_cost =
  function
    Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd), Phi args)
    when 1 < least_cost ->
      let tcosts = 1
      and tkids = []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 0; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd];
         uses = List.filter Ast.is_register (Array.to_list args);
         factored_vars = []; insn = Iphi (rd, args)},
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when cost_int_node rm + 1 < least_cost ->
      let tcosts = cost_int_node rm + 1
      and tkids = kids_int_node rm []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 1; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu2F
                   (Iformat.Mov, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm)])},
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction {ruleno = 3; cost_with_kids = fk3; factored_vars = [rm]})
    when fk3 + 1 < least_cost ->
      let tcosts = fk3 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 2; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu2F
                   (Iformat.Not, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm)])},
      tkids
  | Unop
      (Not,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when cost_int_node rm + 2 < least_cost ->
      let tcosts = cost_int_node rm + 2
      and tkids = kids_int_node rm []
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 3; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu2F
                    (Iformat.Not, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction {ruleno = 5; cost_with_kids = fk5; factored_vars = [rm; rn]})
    when fk5 + 1 < least_cost ->
      let tcosts = fk5 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 4; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Lsl, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Lsl,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 5; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Lsl, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction {ruleno = 7; cost_with_kids = fk7; factored_vars = [rm; c]})
    when fk7 + 1 < least_cost && match_shiftimm c ->
      let tcosts = fk7 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 6; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Lsl, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Lsl,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk11;
          factored_vars = [c]})
    when cost_int_node rm + (fk11 + 2) < least_cost && match_shiftimm c ->
      let tcosts = cost_int_node rm + (fk11 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 7; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Lsl, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 9; cost_with_kids = fk13; factored_vars = [rm; rn]})
    when fk13 + 1 < least_cost ->
      let tcosts = fk13 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 8; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Lsr, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Lsr,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 9; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Lsr, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 11; cost_with_kids = fk15; factored_vars = [rm; c]})
    when fk15 + 1 < least_cost && match_shiftimm c ->
      let tcosts = fk15 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 10; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Lsr, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Lsr,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk19;
          factored_vars = [c]})
    when cost_int_node rm + (fk19 + 2) < least_cost && match_shiftimm c ->
      let tcosts = cost_int_node rm + (fk19 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 11; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Lsr, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 13; cost_with_kids = fk21; factored_vars = [rm; rn]})
    when fk21 + 1 < least_cost ->
      let tcosts = fk21 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 12; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Asr, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Asr,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 13; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Asr, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 15; cost_with_kids = fk23; factored_vars = [rm; c]})
    when fk23 + 1 < least_cost && match_shiftimm c ->
      let tcosts = fk23 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 14; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Asr, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Asr,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk27;
          factored_vars = [c]})
    when cost_int_node rm + (fk27 + 2) < least_cost && match_shiftimm c ->
      let tcosts = cost_int_node rm + (fk27 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 15; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Asr, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 17; cost_with_kids = fk29; factored_vars = [rm; rn]})
    when fk29 + 1 < least_cost ->
      let tcosts = fk29 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 16; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Ror, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Ror,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 17; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Ror, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 19; cost_with_kids = fk31; factored_vars = [rm; c]})
    when fk31 + 1 < least_cost && match_shiftimm c ->
      let tcosts = fk31 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 18; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Ror, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Ror,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk35;
          factored_vars = [c]})
    when cost_int_node rm + (fk35 + 2) < least_cost && match_shiftimm c ->
      let tcosts = cost_int_node rm + (fk35 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 19; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Ror, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 21; cost_with_kids = fk37; factored_vars = [rm; rn]})
    when fk37 + 1 < least_cost ->
      let tcosts = fk37 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 20; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.And, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (And,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 21; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.And, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 23; cost_with_kids = fk39; factored_vars = [rm; c]})
    when fk39 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk39 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 22; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.And, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (And,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk43;
          factored_vars = [c]})
    when cost_int_node rm + (fk43 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk43 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 23; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.And, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 25; cost_with_kids = fk45; factored_vars = [c; rm]})
    when fk45 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk45 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 24; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.And, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (And,
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk49;
          factored_vars = [c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when fk49 + (cost_int_node rm + 2) < least_cost && match_dataimm c ->
      let tcosts = fk49 + (cost_int_node rm + 2)
      and tkids = (fun x -> x) (kids_int_node rm [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 25; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [c; rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.And, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 27; cost_with_kids = fk51; factored_vars = [rm; rn]})
    when fk51 + 1 < least_cost ->
      let tcosts = fk51 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 26; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Ior, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Ior,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 27; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Ior, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 29; cost_with_kids = fk53; factored_vars = [rm; c]})
    when fk53 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk53 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 28; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Ior, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Ior,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk57;
          factored_vars = [c]})
    when cost_int_node rm + (fk57 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk57 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 29; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Ior, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 31; cost_with_kids = fk59; factored_vars = [c; rm]})
    when fk59 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk59 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 30; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Ior, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Ior,
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk63;
          factored_vars = [c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when fk63 + (cost_int_node rm + 2) < least_cost && match_dataimm c ->
      let tcosts = fk63 + (cost_int_node rm + 2)
      and tkids = (fun x -> x) (kids_int_node rm [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 31; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [c; rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Ior, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 33; cost_with_kids = fk65; factored_vars = [rm; rn]})
    when fk65 + 1 < least_cost ->
      let tcosts = fk65 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 32; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Eor, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Eor,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 33; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Eor, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 35 | 37; cost_with_kids = fk67; factored_vars = [rm; c]})
    when fk67 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk67 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 34; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Eor, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Eor,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk71;
          factored_vars = [c]})
    when cost_int_node rm + (fk71 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk71 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 35; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Eor, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Binop
         (Eor,
          Reduction
            {ruleno = 91 | (93 | 99);
             cost_with_kids = fk73;
             factored_vars = [c]},
          (Register (_, (Id.IntType | Id.PtrType), _) |
           Pseudo (_, (Id.IntType | Id.PtrType), _) |
           Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as
             rm)))
    when fk73 + (cost_int_node rm + 1) < least_cost && match_dataimm c ->
      let tcosts = fk73 + (cost_int_node rm + 1)
      and tkids = (fun x -> x) (kids_int_node rm [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 36; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Eor, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Eor,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk75;
          factored_vars = [c]})
    when cost_int_node rm + (fk75 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk75 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 37; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Eor, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 39; cost_with_kids = fk77; factored_vars = [rm; rn]})
    when fk77 + 1 < least_cost ->
      let tcosts = fk77 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 38; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Bic, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (And,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction {ruleno = 3; cost_with_kids = fk81; factored_vars = [rn]})
    when cost_int_node rm + (fk81 + 2) < least_cost ->
      let tcosts = cost_int_node rm + (fk81 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 39; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Bic, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 41; cost_with_kids = fk83; factored_vars = [rm; c]})
    when fk83 + 1 < least_cost && match_invdataimm c ->
      let tcosts = fk83 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 40; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Bic, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (And,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 95 | (97 | 99);
          cost_with_kids = fk87;
          factored_vars = [c]})
    when cost_int_node rm + (fk87 + 2) < least_cost && match_invdataimm c ->
      let tcosts = cost_int_node rm + (fk87 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 41; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Bic, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 43; cost_with_kids = fk89; factored_vars = [rm; rn]})
    when fk89 + 1 < least_cost ->
      let tcosts = fk89 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 42; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Add, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Add,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 43; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Add, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 45; cost_with_kids = fk91; factored_vars = [rm; c]})
    when fk91 + 2 < least_cost && match_maybenegdataimm c ->
      let tcosts = fk91 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 44; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (if match_negdataimm c then
                let n = negate_ast_const c in
                fun alloc ->
                  [Iformat.Alu3iF
                     (Iformat.Sub, lookup_destreg alloc rd,
                      lookup_srcreg alloc rm, get_const n)]
              else
                fun alloc ->
                  [Iformat.Alu3iF
                     (Iformat.Add, lookup_destreg alloc rd,
                      lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Add,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk95;
          factored_vars = [c]})
    when
      cost_int_node rm + (fk95 + 3) < least_cost && match_maybenegdataimm c ->
      let tcosts = cost_int_node rm + (fk95 + 3)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 45; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (if match_negdataimm c then
                 let n = negate_ast_const c in
                 fun alloc ->
                   [Iformat.Alu3iF
                      (Iformat.Sub, lookup_destreg alloc rt,
                       lookup_srcreg alloc rm, get_const n)]
               else
                 fun alloc ->
                   [Iformat.Alu3iF
                      (Iformat.Add, lookup_destreg alloc rt,
                       lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 47; cost_with_kids = fk97; factored_vars = [c; rm]})
    when fk97 + 2 < least_cost && match_maybenegdataimm c ->
      let tcosts = fk97 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 46; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (if match_negdataimm c then
                let n = negate_ast_const c in
                fun alloc ->
                  [Iformat.Alu3iF
                     (Iformat.Sub, lookup_destreg alloc rd,
                      lookup_srcreg alloc rm, get_const n)]
              else
                fun alloc ->
                  [Iformat.Alu3iF
                     (Iformat.Add, lookup_destreg alloc rd,
                      lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Add,
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk101;
          factored_vars = [c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when
      fk101 + (cost_int_node rm + 3) < least_cost &&
      match_maybenegdataimm c ->
      let tcosts = fk101 + (cost_int_node rm + 3)
      and tkids = (fun x -> x) (kids_int_node rm [])
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 47; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [c; rm];
          insn =
            Insn
              (if match_negdataimm c then
                 let n = negate_ast_const c in
                 fun alloc ->
                   [Iformat.Alu3iF
                      (Iformat.Sub, lookup_destreg alloc rt,
                       lookup_srcreg alloc rm, get_const n)]
               else
                 fun alloc ->
                   [Iformat.Alu3iF
                      (Iformat.Add, lookup_destreg alloc rt,
                       lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 49; cost_with_kids = fk103; factored_vars = [rm; rn]})
    when fk103 + 1 < least_cost ->
      let tcosts = fk103 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 48; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Sub, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Sub,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 49; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Sub, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 51; cost_with_kids = fk105; factored_vars = [rm; c]})
    when fk105 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk105 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 50; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Sub, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Sub,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk109;
          factored_vars = [c]})
    when cost_int_node rm + (fk109 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk109 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 51; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Sub, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 53; cost_with_kids = fk111; factored_vars = [c; rm]})
    when fk111 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk111 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 52; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Rsb, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Sub,
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk115;
          factored_vars = [c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when fk115 + (cost_int_node rm + 2) < least_cost && match_dataimm c ->
      let tcosts = fk115 + (cost_int_node rm + 2)
      and tkids = (fun x -> x) (kids_int_node rm [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 53; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [c; rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Rsb, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 55; cost_with_kids = fk117; factored_vars = [rm; rn]})
    when fk117 + 1 < least_cost ->
      let tcosts = fk117 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 54; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Mul, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Mul,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 55; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Mul, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 57; cost_with_kids = fk119; factored_vars = [rm; c]})
    when fk119 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk119 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 56; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Mul, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Mul,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk123;
          factored_vars = [c]})
    when cost_int_node rm + (fk123 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk123 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 57; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Mul, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 59; cost_with_kids = fk125; factored_vars = [c; rm]})
    when fk125 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk125 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 58; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Mul, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Mul,
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk129;
          factored_vars = [c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when fk129 + (cost_int_node rm + 2) < least_cost && match_dataimm c ->
      let tcosts = fk129 + (cost_int_node rm + 2)
      and tkids = (fun x -> x) (kids_int_node rm [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 59; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [c; rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Mul, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 61; cost_with_kids = fk131; factored_vars = [rm; rn]})
    when fk131 + 1 < least_cost ->
      let tcosts = fk131 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 60; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Div, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Div,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 61; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Div, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 63; cost_with_kids = fk133; factored_vars = [rm; c]})
    when fk133 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk133 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 62; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Div, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Div,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk137;
          factored_vars = [c]})
    when cost_int_node rm + (fk137 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk137 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 63; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Div, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 65; cost_with_kids = fk139; factored_vars = [rm; rn]})
    when fk139 + 1 < least_cost ->
      let tcosts = fk139 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 64; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Udiv, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Udiv,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 65; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Udiv, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 67; cost_with_kids = fk141; factored_vars = [rm; c]})
    when fk141 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk141 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 66; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Udiv, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Udiv,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk145;
          factored_vars = [c]})
    when cost_int_node rm + (fk145 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk145 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 67; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Udiv, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 69; cost_with_kids = fk147; factored_vars = [rm; rn]})
    when fk147 + 1 < least_cost ->
      let tcosts = fk147 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 68; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Mod, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Mod,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 69; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Mod, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 71; cost_with_kids = fk149; factored_vars = [rm; c]})
    when fk149 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk149 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 70; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Mod, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Mod,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk153;
          factored_vars = [c]})
    when cost_int_node rm + (fk153 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk153 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 71; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Mod, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 73; cost_with_kids = fk155; factored_vars = [rm; rn]})
    when fk155 + 1 < least_cost ->
      let tcosts = fk155 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 72; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3F
                   (Iformat.Umod, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Umod,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 73; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3F
                    (Iformat.Umod, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 75; cost_with_kids = fk157; factored_vars = [rm; c]})
    when fk157 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk157 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 74; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3iF
                   (Iformat.Umod, lookup_destreg alloc rd,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Umod,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk161;
          factored_vars = [c]})
    when cost_int_node rm + (fk161 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk161 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 75; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.Alu3iF
                    (Iformat.Umod, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Binop
         (Ior,
          Binop
            (And,
             (Register (_, (Id.IntType | Id.PtrType), _) |
              Pseudo (_, (Id.IntType | Id.PtrType), _) |
              Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)}
                as re), (Constant _ as n162)),
          Reduction
            {ruleno = 91 | 99; cost_with_kids = fk164; factored_vars = [c]}))
    when
      cost_int_node re + (fk164 + 1) < least_cost &&
      n162 = Constant (Int32.of_string "65535") && match_ehimm c ->
      let tcosts = cost_int_node re + (fk164 + 1)
      and tkids = kids_int_node re ((fun x -> x) [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 76; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [re];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Zeros,
                    Iformat.Merge, Iformat.Hi)])},
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Binop
         (Ior,
          Binop
            (And,
             (Register (_, (Id.IntType | Id.PtrType), _) |
              Pseudo (_, (Id.IntType | Id.PtrType), _) |
              Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)}
                as re), (Constant _ as n165)),
          Reduction
            {ruleno = 93 | 99; cost_with_kids = fk167; factored_vars = [c]}))
    when
      cost_int_node re + (fk167 + 1) < least_cost &&
      n165 = Constant (Int32.of_string "-65536") && match_elimm c ->
      let tcosts = cost_int_node re + (fk167 + 1)
      and tkids = kids_int_node re ((fun x -> x) [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 77; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [re];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Zeros,
                    Iformat.Merge, Iformat.Lo)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop
         (Cast_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm)))
    when cost_float_node fm + 1 < least_cost ->
      let tcosts = cost_float_node fm + 1
      and tkids = kids_float_node fm []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 78; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu2fF
                   (Iformat.Movf, Iformat.Single, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop
         (Cast_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm)))
    when cost_float_node fm + 1 < least_cost ->
      let tcosts = cost_float_node fm + 1
      and tkids = kids_float_node fm []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 79; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu2fF
                   (Iformat.Movf, Iformat.Double, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop (Cast_f Iformat.Single, (Floatconst _ as c)))
    when 1 < least_cost && match_floatimm c ->
      let tcosts = 1
      and tkids = []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 80; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu2fiF
                   (Iformat.Movf, Iformat.Single, lookup_destfreg alloc fd,
                    get_const c)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop (Cast_f Iformat.Double, (Floatconst _ as c)))
    when 1 < least_cost && match_floatimm c ->
      let tcosts = 1
      and tkids = []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 81; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu2fiF
                   (Iformat.Movf, Iformat.Double, lookup_destfreg alloc fd,
                    get_const c)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Add_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 82; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Addf, Iformat.Single, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Add_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 83; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Addf, Iformat.Double, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Sub_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 84; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Subf, Iformat.Single, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Sub_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 85; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Subf, Iformat.Double, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Mul_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 86; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Mulf, Iformat.Single, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Mul_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 87; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Mulf, Iformat.Double, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Div_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 88; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Divf, Iformat.Single, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Binop
         (Div_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fm),
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fn)))
    when cost_float_node fm + (cost_float_node fn + 1) < least_cost ->
      let tcosts = cost_float_node fm + (cost_float_node fn + 1)
      and tkids = kids_float_node fm (kids_float_node fn [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 89; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [fn; fm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.Alu3fF
                   (Iformat.Divf, Iformat.Double, lookup_destfreg alloc fd,
                    lookup_srcfreg alloc fm, lookup_srcfreg alloc fn)])},
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 91 | 99; cost_with_kids = fk169; factored_vars = [c]})
    when fk169 + 2 < least_cost && match_ehimm c ->
      let tcosts = fk169 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 90; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Zeros,
                    Iformat.Replace, Iformat.Hi)])},
      tkids
  | Constant _ as c when 3 < least_cost && match_ehimm c ->
      let tcosts = 3
      and tkids = []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 91; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [];
          factored_vars = [c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.MvcF
                    (lookup_destreg alloc rt, get_const c, Iformat.Zeros,
                     Iformat.Replace, Iformat.Hi)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 93 | 99; cost_with_kids = fk171; factored_vars = [c]})
    when fk171 + 2 < least_cost && match_elimm c ->
      let tcosts = fk171 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 92; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Zeros,
                    Iformat.Replace, Iformat.Lo)])},
      tkids
  | Constant _ as c when 3 < least_cost && match_elimm c ->
      let tcosts = 3
      and tkids = []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 93; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [];
          factored_vars = [c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.MvcF
                    (lookup_destreg alloc rt, get_const c, Iformat.Zeros,
                     Iformat.Replace, Iformat.Lo)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 95 | 99; cost_with_kids = fk173; factored_vars = [c]})
    when fk173 + 1 < least_cost && match_fhimm c ->
      let tcosts = fk173 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 94; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Ones,
                    Iformat.Replace, Iformat.Hi)])},
      tkids
  | Constant _ as c when 3 < least_cost && match_fhimm c ->
      let tcosts = 3
      and tkids = []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 95; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [];
          factored_vars = [c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.MvcF
                    (lookup_destreg alloc rt, get_const c, Iformat.Ones,
                     Iformat.Replace, Iformat.Hi)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 97 | 99; cost_with_kids = fk175; factored_vars = [c]})
    when fk175 + 1 < least_cost && match_flimm c ->
      let tcosts = fk175 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 96; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Ones,
                    Iformat.Replace, Iformat.Lo)])},
      tkids
  | Constant _ as c when 3 < least_cost && match_flimm c ->
      let tcosts = 3
      and tkids = []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 97; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [];
          factored_vars = [c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.MvcF
                    (lookup_destreg alloc rt, get_const c, Iformat.Ones,
                     Iformat.Replace, Iformat.Lo)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk177;
          factored_vars = [c]})
    when fk177 + 2 < least_cost ->
      let tcosts = fk177 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 98; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Zeros,
                    Iformat.Merge, Iformat.Lo);
                 Iformat.MvcF
                   (lookup_destreg alloc rd, get_const c, Iformat.Zeros,
                    Iformat.Merge, Iformat.Hi)])},
      tkids
  | Constant _ as c when 3 < least_cost ->
      let tcosts = 3
      and tkids = []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 99; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [];
          factored_vars = [c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.MvcF
                    (lookup_destreg alloc rt, get_const c, Iformat.Zeros,
                     Iformat.Merge, Iformat.Lo);
                  Iformat.MvcF
                    (lookup_destreg alloc rt, get_const c, Iformat.Zeros,
                     Iformat.Merge, Iformat.Hi)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction {ruleno = 101; cost_with_kids = fk179; factored_vars = [rb]})
    when fk179 + 2 < least_cost ->
      let tcosts = fk179 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 100; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Word)])},
      tkids
  | Unop
      (Ind Ast.Word,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rb
          ))
    when cost_int_node rb + 3 < least_cost ->
      let tcosts = cost_int_node rb + 3
      and tkids = kids_int_node rb []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 101; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [rb];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     Int32.of_string "0", Iformat.Nonvolatile,
                     Iformat.Word)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 103; cost_with_kids = fk181; factored_vars = [rb; ri]})
    when fk181 + 2 < least_cost ->
      let tcosts = fk181 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 102; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdrF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Word)])},
      tkids
  | Unop
      (Ind Ast.Word,
       Reduction
         {ruleno = 43; cost_with_kids = fk185; factored_vars = [rb; ri]})
    when fk185 + 3 < least_cost ->
      let tcosts = fk185 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 103; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [ri; rb];
          factored_vars = [rb; ri];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdrF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     lookup_srcreg alloc ri, Iformat.Nonvolatile,
                     Iformat.Word)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 105; cost_with_kids = fk187; factored_vars = [rb; c]})
    when fk187 + 2 < least_cost && match_wmimm c ->
      let tcosts = fk187 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 104; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Word)])},
      tkids
  | Unop
      (Ind Ast.Word,
       Reduction
         {ruleno = 45; cost_with_kids = fk193; factored_vars = [rb; c]})
    when fk193 + 3 < least_cost && match_wmimm c ->
      let tcosts = fk193 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 105; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [rb; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     get_const c, Iformat.Nonvolatile, Iformat.Word)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 107; cost_with_kids = fk197; factored_vars = [c; rb]})
    when fk197 + 2 < least_cost && match_wmimm c ->
      let tcosts = fk197 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 106; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Word)])},
      tkids
  | Unop
      (Ind Ast.Word,
       Reduction
         {ruleno = 47; cost_with_kids = fk203; factored_vars = [c; rb]})
    when fk203 + 3 < least_cost && match_wmimm c ->
      let tcosts = fk203 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 107; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [c; rb];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     get_const c, Iformat.Nonvolatile, Iformat.Word)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction {ruleno = 109; cost_with_kids = fk207; factored_vars = [rb]})
    when fk207 + 2 < least_cost ->
      let tcosts = fk207 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 108; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Halfword)])},
      tkids
  | Unop
      (Ind Ast.Halfword,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rb
          ))
    when cost_int_node rb + 3 < least_cost ->
      let tcosts = cost_int_node rb + 3
      and tkids = kids_int_node rb []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 109; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [rb];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     Int32.of_string "0", Iformat.Nonvolatile,
                     Iformat.Halfword)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 111; cost_with_kids = fk209; factored_vars = [rb; ri]})
    when fk209 + 2 < least_cost ->
      let tcosts = fk209 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 110; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdrF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Halfword)])},
      tkids
  | Unop
      (Ind Ast.Halfword,
       Reduction
         {ruleno = 43; cost_with_kids = fk213; factored_vars = [rb; ri]})
    when fk213 + 3 < least_cost ->
      let tcosts = fk213 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 111; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [ri; rb];
          factored_vars = [rb; ri];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdrF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     lookup_srcreg alloc ri, Iformat.Nonvolatile,
                     Iformat.Halfword)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 113; cost_with_kids = fk215; factored_vars = [rb; c]})
    when fk215 + 2 < least_cost && match_hmimm c ->
      let tcosts = fk215 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 112; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Halfword)])},
      tkids
  | Unop
      (Ind Ast.Halfword,
       Reduction
         {ruleno = 45; cost_with_kids = fk221; factored_vars = [rb; c]})
    when fk221 + 3 < least_cost && match_hmimm c ->
      let tcosts = fk221 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 113; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [rb; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     get_const c, Iformat.Nonvolatile, Iformat.Halfword)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction {ruleno = 115; cost_with_kids = fk225; factored_vars = [rb]})
    when fk225 + 2 < least_cost ->
      let tcosts = fk225 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 114; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Byte)])},
      tkids
  | Unop
      (Ind Ast.Byte,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rb
          ))
    when cost_int_node rb + 3 < least_cost ->
      let tcosts = cost_int_node rb + 3
      and tkids = kids_int_node rb []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 115; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [rb];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     Int32.of_string "0", Iformat.Nonvolatile,
                     Iformat.Byte)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 117; cost_with_kids = fk227; factored_vars = [rb; ri]})
    when fk227 + 2 < least_cost ->
      let tcosts = fk227 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 116; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdrF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Byte)])},
      tkids
  | Unop
      (Ind Ast.Byte,
       Reduction
         {ruleno = 43; cost_with_kids = fk231; factored_vars = [rb; ri]})
    when fk231 + 3 < least_cost ->
      let tcosts = fk231 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 117; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [ri; rb];
          factored_vars = [rb; ri];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdrF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     lookup_srcreg alloc ri, Iformat.Nonvolatile,
                     Iformat.Byte)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 119; cost_with_kids = fk233; factored_vars = [rb; c]})
    when fk233 + 2 < least_cost && match_bmimm c ->
      let tcosts = fk233 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 118; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdriF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Byte)])},
      tkids
  | Unop
      (Ind Ast.Byte,
       Reduction
         {ruleno = 45; cost_with_kids = fk239; factored_vars = [rb; c]})
    when fk239 + 3 < least_cost && match_bmimm c ->
      let tcosts = fk239 + 3
      and tkids = (fun x -> x) []
      and self_cost = 3 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 119; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rb];
          factored_vars = [rb; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.LdriF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rb,
                     get_const c, Iformat.Nonvolatile, Iformat.Byte)])}),
      tkids
  | Move
      (Reduction {ruleno = 101; cost_with_kids = fk243; factored_vars = [rb]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk243 + (cost_int_node rs + 2) < least_cost ->
      let tcosts = fk243 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 120; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StriF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Word)])},
      tkids
  | Move
      (Reduction
         {ruleno = 103; cost_with_kids = fk245; factored_vars = [rb; ri]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk245 + (cost_int_node rs + 2) < least_cost ->
      let tcosts = fk245 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 121; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StrF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Word)])},
      tkids
  | Move
      (Reduction
         {ruleno = 105; cost_with_kids = fk249; factored_vars = [rb; c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk249 + (cost_int_node rs + 2) < least_cost && match_wmimm c ->
      let tcosts = fk249 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 122; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StriF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Word)])},
      tkids
  | Move
      (Reduction {ruleno = 109; cost_with_kids = fk255; factored_vars = [rb]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk255 + (cost_int_node rs + 2) < least_cost ->
      let tcosts = fk255 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 123; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StriF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Halfword)])},
      tkids
  | Move
      (Reduction
         {ruleno = 111; cost_with_kids = fk257; factored_vars = [rb; ri]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk257 + (cost_int_node rs + 2) < least_cost ->
      let tcosts = fk257 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 124; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StrF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Halfword)])},
      tkids
  | Move
      (Reduction
         {ruleno = 113; cost_with_kids = fk261; factored_vars = [rb; c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk261 + (cost_int_node rs + 2) < least_cost && match_hmimm c ->
      let tcosts = fk261 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 125; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StriF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Halfword)])},
      tkids
  | Move
      (Reduction {ruleno = 115; cost_with_kids = fk267; factored_vars = [rb]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk267 + (cost_int_node rs + 2) < least_cost ->
      let tcosts = fk267 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 126; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StriF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Byte)])},
      tkids
  | Move
      (Reduction
         {ruleno = 117; cost_with_kids = fk269; factored_vars = [rb; ri]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk269 + (cost_int_node rs + 2) < least_cost ->
      let tcosts = fk269 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 127; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StrF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Byte)])},
      tkids
  | Move
      (Reduction
         {ruleno = 119; cost_with_kids = fk273; factored_vars = [rb; c]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rs
          ))
    when fk273 + (cost_int_node rs + 2) < least_cost && match_bmimm c ->
      let tcosts = fk273 + (cost_int_node rs + 2)
      and tkids = (fun x -> x) (kids_int_node rs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 128; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StriF
                   (lookup_srcreg alloc rs, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Byte)])},
      tkids
  | Move
      (Reduction {ruleno = 101; cost_with_kids = fk279; factored_vars = [rb]},
       Unop
         (Cast_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fs)))
    when fk279 + (cost_float_node fs + 2) < least_cost ->
      let tcosts = fk279 + (cost_float_node fs + 2)
      and tkids = (fun x -> x) (kids_float_node fs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 129; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [fs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StfiF
                   (lookup_srcfreg alloc fs, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Single)])},
      tkids
  | Move
      (Reduction
         {ruleno = 103; cost_with_kids = fk281; factored_vars = [rb; ri]},
       Unop
         (Cast_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fs)))
    when fk281 + (cost_float_node fs + 2) < least_cost ->
      let tcosts = fk281 + (cost_float_node fs + 2)
      and tkids = (fun x -> x) (kids_float_node fs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 130; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [fs; ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StfF
                   (lookup_srcfreg alloc fs, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Single)])},
      tkids
  | Move
      (Reduction
         {ruleno = 105; cost_with_kids = fk285; factored_vars = [rb; c]},
       Unop
         (Cast_f Iformat.Single,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fs)))
    when fk285 + (cost_float_node fs + 2) < least_cost && match_wmimm c ->
      let tcosts = fk285 + (cost_float_node fs + 2)
      and tkids = (fun x -> x) (kids_float_node fs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 131; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [fs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StfiF
                   (lookup_srcfreg alloc fs, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Single)])},
      tkids
  | Move
      (Unop
         (Ind Ast.Doubleword,
          (Register (_, (Id.IntType | Id.PtrType), _) |
           Pseudo (_, (Id.IntType | Id.PtrType), _) |
           Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as
             rb)),
       Unop
         (Cast_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fs)))
    when cost_int_node rb + (cost_float_node fs + 2) < least_cost ->
      let tcosts = cost_int_node rb + (cost_float_node fs + 2)
      and tkids = kids_int_node rb (kids_float_node fs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 132; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [fs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StfiF
                   (lookup_srcfreg alloc fs, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Double)])},
      tkids
  | Move
      (Unop
         (Ind Ast.Doubleword,
          Reduction
            {ruleno = 43; cost_with_kids = fk291; factored_vars = [rb; ri]}),
       Unop
         (Cast_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fs)))
    when fk291 + (cost_float_node fs + 2) < least_cost ->
      let tcosts = fk291 + (cost_float_node fs + 2)
      and tkids = (fun x -> x) (kids_float_node fs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 133; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [fs; ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StfF
                   (lookup_srcfreg alloc fs, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Double)])},
      tkids
  | Move
      (Unop
         (Ind Ast.Doubleword,
          Reduction
            {ruleno = 45; cost_with_kids = fk293; factored_vars = [rb; c]}),
       Unop
         (Cast_f Iformat.Double,
          (Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) |
           Reduction {reduction = Pseudo (_, Id.FloatType, _)} as fs)))
    when fk293 + (cost_float_node fs + 2) < least_cost && match_wmimm c ->
      let tcosts = fk293 + (cost_float_node fs + 2)
      and tkids = (fun x -> x) (kids_float_node fs [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 134; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [fs; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.StfiF
                   (lookup_srcfreg alloc fs, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Double)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Reduction {ruleno = 101; cost_with_kids = fk297; factored_vars = [rb]})
    when fk297 + 2 < least_cost ->
      let tcosts = fk297 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 135; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdfiF
                   (lookup_destfreg alloc fd, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Single)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Reduction
         {ruleno = 103; cost_with_kids = fk299; factored_vars = [rb; ri]})
    when fk299 + 2 < least_cost ->
      let tcosts = fk299 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 136; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdfF
                   (lookup_destfreg alloc fd, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Single)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Reduction
         {ruleno = 105; cost_with_kids = fk303; factored_vars = [rb; c]})
    when fk303 + 2 < least_cost && match_wmimm c ->
      let tcosts = fk303 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 137; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdfiF
                   (lookup_destfreg alloc fd, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Single)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop
         (Ind Ast.Doubleword,
          (Register (_, (Id.IntType | Id.PtrType), _) |
           Pseudo (_, (Id.IntType | Id.PtrType), _) |
           Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as
             rb)))
    when cost_int_node rb + 2 < least_cost ->
      let tcosts = cost_int_node rb + 2
      and tkids = kids_int_node rb []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 138; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdfiF
                   (lookup_destfreg alloc fd, lookup_srcreg alloc rb,
                    Int32.of_string "0", Iformat.Nonvolatile,
                    Iformat.Double)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop
         (Ind Ast.Doubleword,
          Reduction
            {ruleno = 43; cost_with_kids = fk309; factored_vars = [rb; ri]}))
    when fk309 + 2 < least_cost ->
      let tcosts = fk309 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 139; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [ri; rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdfF
                   (lookup_destfreg alloc fd, lookup_srcreg alloc rb,
                    lookup_srcreg alloc ri, Iformat.Nonvolatile,
                    Iformat.Double)])},
      tkids
  | Move
      ((Register (_, Id.FloatType, _) | Pseudo (_, Id.FloatType, _) as fd),
       Unop
         (Ind Ast.Doubleword,
          Reduction
            {ruleno = 45; cost_with_kids = fk311; factored_vars = [rb; c]}))
    when fk311 + 2 < least_cost && match_wmimm c ->
      let tcosts = fk311 + 2
      and tkids = (fun x -> x) []
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 140; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [fd]; uses = [rb];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdfiF
                   (lookup_destfreg alloc fd, lookup_srcreg alloc rb,
                    get_const c, Iformat.Nonvolatile, Iformat.Double)])},
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 142; cost_with_kids = fk315; factored_vars = [rm; rn]})
    when fk315 + 1 < least_cost ->
      let tcosts = fk315 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 141; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Eq, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Eq,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 142; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Eq, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 144; cost_with_kids = fk317; factored_vars = [rm; c]})
    when fk317 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk317 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 143; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Eq, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Eq,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk321;
          factored_vars = [c]})
    when cost_int_node rm + (fk321 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk321 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 144; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Eq, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 146; cost_with_kids = fk323; factored_vars = [rm; rn]})
    when fk323 + 1 < least_cost ->
      let tcosts = fk323 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 145; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Ne, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Ne,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 146; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Ne, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 148; cost_with_kids = fk325; factored_vars = [rm; c]})
    when fk325 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk325 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 147; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Ne, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Ne,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk329;
          factored_vars = [c]})
    when cost_int_node rm + (fk329 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk329 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 148; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Ne, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 150; cost_with_kids = fk331; factored_vars = [rm; rn]})
    when fk331 + 1 < least_cost ->
      let tcosts = fk331 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 149; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Le, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Le,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 150; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Le, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 152; cost_with_kids = fk333; factored_vars = [rm; c]})
    when fk333 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk333 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 151; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Le, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Le,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk337;
          factored_vars = [c]})
    when cost_int_node rm + (fk337 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk337 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 152; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Le, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 154; cost_with_kids = fk339; factored_vars = [rm; rn]})
    when fk339 + 1 < least_cost ->
      let tcosts = fk339 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 153; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Lt, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Lt,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 154; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Lt, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 156; cost_with_kids = fk341; factored_vars = [rm; c]})
    when fk341 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk341 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 155; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Lt, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Lt,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk345;
          factored_vars = [c]})
    when cost_int_node rm + (fk345 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk345 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 156; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Lt, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 158; cost_with_kids = fk347; factored_vars = [rm; rn]})
    when fk347 + 1 < least_cost ->
      let tcosts = fk347 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 157; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Ge, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Ge,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 158; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Ge, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 160; cost_with_kids = fk349; factored_vars = [rm; c]})
    when fk349 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk349 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 159; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Ge, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Ge,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk353;
          factored_vars = [c]})
    when cost_int_node rm + (fk353 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk353 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 160; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Ge, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 162; cost_with_kids = fk355; factored_vars = [rm; rn]})
    when fk355 + 1 < least_cost ->
      let tcosts = fk355 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 161; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Gt, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Gt,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 162; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Gt, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 164; cost_with_kids = fk357; factored_vars = [rm; c]})
    when fk357 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk357 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 163; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Gt, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Gt,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk361;
          factored_vars = [c]})
    when cost_int_node rm + (fk361 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk361 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 164; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Gt, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 166; cost_with_kids = fk363; factored_vars = [rm; rn]})
    when fk363 + 1 < least_cost ->
      let tcosts = fk363 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 165; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Leu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Leu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 166; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Leu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 168; cost_with_kids = fk365; factored_vars = [rm; c]})
    when fk365 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk365 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 167; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Leu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Leu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk369;
          factored_vars = [c]})
    when cost_int_node rm + (fk369 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk369 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 168; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Leu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 170; cost_with_kids = fk371; factored_vars = [rm; rn]})
    when fk371 + 1 < least_cost ->
      let tcosts = fk371 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 169; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Ltu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Ltu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 170; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Ltu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 172; cost_with_kids = fk373; factored_vars = [rm; c]})
    when fk373 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk373 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 171; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Ltu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Ltu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk377;
          factored_vars = [c]})
    when cost_int_node rm + (fk377 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk377 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 172; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Ltu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 174; cost_with_kids = fk379; factored_vars = [rm; rn]})
    when fk379 + 1 < least_cost ->
      let tcosts = fk379 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 173; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Geu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Geu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 174; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Geu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 176; cost_with_kids = fk381; factored_vars = [rm; c]})
    when fk381 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk381 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 175; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Geu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Geu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk385;
          factored_vars = [c]})
    when cost_int_node rm + (fk385 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk385 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 176; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Geu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 178; cost_with_kids = fk387; factored_vars = [rm; rn]})
    when fk387 + 1 < least_cost ->
      let tcosts = fk387 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 177; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rn; rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpF
                   (Iformat.Gtu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, lookup_srcreg alloc rn)])},
      tkids
  | Binop
      (Cmp Iformat.Gtu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rn
          ))
    when cost_int_node rm + (cost_int_node rn + 2) < least_cost ->
      let tcosts = cost_int_node rm + (cost_int_node rn + 2)
      and tkids = kids_int_node rm (kids_int_node rn [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 178; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rn; rm];
          factored_vars = [rm; rn];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpF
                    (Iformat.Gtu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, lookup_srcreg alloc rn)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rc),
       Reduction
         {ruleno = 180; cost_with_kids = fk389; factored_vars = [rm; c]})
    when fk389 + 1 < least_cost && match_dataimm c ->
      let tcosts = fk389 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 179; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rc]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CmpiF
                   (Iformat.Gtu, lookup_destreg alloc rc,
                    lookup_srcreg alloc rm, get_const c)])},
      tkids
  | Binop
      (Cmp Iformat.Gtu,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ),
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk393;
          factored_vars = [c]})
    when cost_int_node rm + (fk393 + 2) < least_cost && match_dataimm c ->
      let tcosts = cost_int_node rm + (fk393 + 2)
      and tkids = kids_int_node rm ((fun x -> x) [])
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 180; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [rm; c];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.CmpiF
                    (Iformat.Gtu, lookup_destreg alloc rt,
                     lookup_srcreg alloc rm, get_const c)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 182; cost_with_kids = fk395; factored_vars = [a; b; rm]})
    when fk395 + 1 < least_cost && match_shiftimm a && match_shiftimm b ->
      let tcosts = fk395 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 181; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.BfxF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rm,
                    get_num a, get_num b, Iformat.Zeroext)])},
      tkids
  | Triop
      (Bitfield,
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk401;
          factored_vars = [a]},
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk403;
          factored_vars = [b]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when
      fk401 + (fk403 + (cost_int_node rm + 2)) < least_cost &&
      match_shiftimm a && match_shiftimm b ->
      let tcosts = fk401 + (fk403 + (cost_int_node rm + 2))
      and tkids = (fun x -> x) ((fun x -> x) (kids_int_node rm []))
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 182; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [a; b; rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.BfxF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rm,
                     get_num a, get_num b, Iformat.Zeroext)])}),
      tkids
  | Move
      ((Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) as rd),
       Reduction
         {ruleno = 184; cost_with_kids = fk405; factored_vars = [a; b; rm]})
    when fk405 + 1 < least_cost && match_shiftimm a && match_shiftimm b ->
      let tcosts = fk405 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 183; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = [rd]; uses = [rm];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.BfxF
                   (lookup_destreg alloc rd, lookup_srcreg alloc rm,
                    get_num a, get_num b, Iformat.Signext)])},
      tkids
  | Triop
      (SignedBitfield,
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk411;
          factored_vars = [a]},
       Reduction
         {ruleno = 91 | (93 | 99);
          cost_with_kids = fk413;
          factored_vars = [b]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rm
          ))
    when
      fk411 + (fk413 + (cost_int_node rm + 2)) < least_cost &&
      match_shiftimm a && match_shiftimm b ->
      let tcosts = fk411 + (fk413 + (cost_int_node rm + 2))
      and tkids = (fun x -> x) ((fun x -> x) (kids_int_node rm []))
      and self_cost = 2 in
      (let rt = gen_int_pseudo () in
       Reduction
         {reduction = rt; ruleno = 184; cost = tcosts;
          cost_with_kids = tcosts - self_cost; defs = [rt]; uses = [rm];
          factored_vars = [a; b; rm];
          insn =
            Insn
              (fun alloc ->
                 [Iformat.BfxF
                    (lookup_destreg alloc rt, lookup_srcreg alloc rm,
                     get_num a, get_num b, Iformat.Signext)])}),
      tkids
  | Binop
      (Trap Iformat.OnZero,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk415;
          factored_vars = [h]})
    when cost_int_node rc + (fk415 + 2) < least_cost ->
      let tcosts = cost_int_node rc + (fk415 + 2)
      and tkids = kids_int_node rc ((fun x -> x) [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 185; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldz, split_lobits h);
                 Iformat.TrapF
                   (Iformat.OnZero, lookup_srcreg alloc rc,
                    Iformat.Bits (split_hibits h))])},
      tkids
  | Binop
      (Trap Iformat.OnZero,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rh
          ))
    when cost_int_node rc + (cost_int_node rh + 2) < least_cost ->
      let tcosts = cost_int_node rc + (cost_int_node rh + 2)
      and tkids = kids_int_node rc (kids_int_node rh [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 186; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rh; rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.TrapF
                   (Iformat.OnZero, lookup_srcreg alloc rc,
                    Iformat.Srcreg (lookup_srcreg alloc rh))])},
      tkids
  | Binop
      (Trap Iformat.OnNonzero,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk417;
          factored_vars = [h]})
    when cost_int_node rc + (fk417 + 2) < least_cost ->
      let tcosts = cost_int_node rc + (fk417 + 2)
      and tkids = kids_int_node rc ((fun x -> x) [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 187; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldz, split_lobits h);
                 Iformat.TrapF
                   (Iformat.OnNonzero, lookup_srcreg alloc rc,
                    Iformat.Bits (split_hibits h))])},
      tkids
  | Binop
      (Trap Iformat.OnNonzero,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rh
          ))
    when cost_int_node rc + (cost_int_node rh + 2) < least_cost ->
      let tcosts = cost_int_node rc + (cost_int_node rh + 2)
      and tkids = kids_int_node rc (kids_int_node rh [])
      and self_cost = 2 in
      Reduction
        {reduction = Null; ruleno = 188; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rh; rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.TrapF
                   (Iformat.OnNonzero, lookup_srcreg alloc rc,
                    Iformat.Srcreg (lookup_srcreg alloc rh))])},
      tkids
  | Triop
      (Branch,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk419;
          factored_vars = [t]},
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk421;
          factored_vars = [f]})
    when cost_int_node rc + (fk419 + (fk421 + 1)) < least_cost ->
      let tcosts = cost_int_node rc + (fk419 + (fk421 + 1))
      and tkids = kids_int_node rc ((fun x -> x) ((fun x -> x) []))
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 189; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldx, split_lobits t);
                 Iformat.LdtF (Iformat.Ldy, split_lobits f);
                 Iformat.CbrF
                   (lookup_srcreg alloc rc, Iformat.Bits (split_hibits t),
                    Iformat.Bits (split_hibits f))])},
      tkids
  | Triop
      (Branch,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rt
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk423;
          factored_vars = [f]})
    when cost_int_node rc + (cost_int_node rt + (fk423 + 1)) < least_cost ->
      let tcosts = cost_int_node rc + (cost_int_node rt + (fk423 + 1))
      and tkids = kids_int_node rc (kids_int_node rt ((fun x -> x) []))
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 190; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rt; rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldy, split_lobits f);
                 Iformat.CbrF
                   (lookup_srcreg alloc rc,
                    Iformat.Srcreg (lookup_srcreg alloc rt),
                    Iformat.Bits (split_hibits f))])},
      tkids
  | Triop
      (Branch,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk425;
          factored_vars = [t]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rf
          ))
    when cost_int_node rc + (fk425 + (cost_int_node rf + 1)) < least_cost ->
      let tcosts = cost_int_node rc + (fk425 + (cost_int_node rf + 1))
      and tkids = kids_int_node rc ((fun x -> x) (kids_int_node rf []))
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 191; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rf; rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldx, split_lobits t);
                 Iformat.CbrF
                   (lookup_srcreg alloc rc, Iformat.Bits (split_hibits t),
                    Iformat.Srcreg (lookup_srcreg alloc rf))])},
      tkids
  | Triop
      (Branch,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rc
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rt
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rf
          ))
    when
      cost_int_node rc + (cost_int_node rt + (cost_int_node rf + 1)) <
        least_cost ->
      let tcosts =
        cost_int_node rc + (cost_int_node rt + (cost_int_node rf + 1))
      and tkids = kids_int_node rc (kids_int_node rt (kids_int_node rf []))
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 192; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rf; rt; rc];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CbrF
                   (lookup_srcreg alloc rc,
                    Iformat.Srcreg (lookup_srcreg alloc rt),
                    Iformat.Srcreg (lookup_srcreg alloc rf))])},
      tkids
  | Binop
      (Call,
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk427;
          factored_vars = [t]},
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk429;
          factored_vars = [f]})
    when fk427 + (fk429 + 1) < least_cost ->
      let tcosts = fk427 + (fk429 + 1)
      and tkids = (fun x -> x) ((fun x -> x) [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 193; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldx, split_lobits t);
                 Iformat.LdtF (Iformat.Ldy, split_lobits f);
                 Iformat.CallF
                   (Iformat.Bits (split_hibits t),
                    Iformat.Bits (split_hibits f))])},
      tkids
  | Binop
      (Call,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rt
          ),
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk431;
          factored_vars = [f]})
    when cost_int_node rt + (fk431 + 1) < least_cost ->
      let tcosts = cost_int_node rt + (fk431 + 1)
      and tkids = kids_int_node rt ((fun x -> x) [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 194; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rt];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldy, split_lobits f);
                 Iformat.CallF
                   (Iformat.Srcreg (lookup_srcreg alloc rt),
                    Iformat.Bits (split_hibits f))])},
      tkids
  | Binop
      (Call,
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk433;
          factored_vars = [t]},
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rf
          ))
    when fk433 + (cost_int_node rf + 1) < least_cost ->
      let tcosts = fk433 + (cost_int_node rf + 1)
      and tkids = (fun x -> x) (kids_int_node rf [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 195; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rf];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldx, split_lobits t);
                 Iformat.CallF
                   (Iformat.Bits (split_hibits t),
                    Iformat.Srcreg (lookup_srcreg alloc rf))])},
      tkids
  | Binop
      (Call,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rt
          ),
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rf
          ))
    when cost_int_node rt + (cost_int_node rf + 1) < least_cost ->
      let tcosts = cost_int_node rt + (cost_int_node rf + 1)
      and tkids = kids_int_node rt (kids_int_node rf [])
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 196; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rf; rt];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.CallF
                   (Iformat.Srcreg (lookup_srcreg alloc rt),
                    Iformat.Srcreg (lookup_srcreg alloc rf))])},
      tkids
  | Unop
      (Jump,
       Reduction
         {ruleno = 91 | (93 | (95 | (97 | 99)));
          cost_with_kids = fk435;
          factored_vars = [j]})
    when fk435 + 1 < least_cost ->
      let tcosts = fk435 + 1
      and tkids = (fun x -> x) []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 197; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.LdtF (Iformat.Ldx, split_lobits j);
                 Iformat.JumpF (Iformat.Bits (split_hibits j))])},
      tkids
  | Unop
      (Jump,
       (Register (_, (Id.IntType | Id.PtrType), _) |
        Pseudo (_, (Id.IntType | Id.PtrType), _) |
        Reduction {reduction = Pseudo (_, (Id.IntType | Id.PtrType), _)} as rj
          ))
    when cost_int_node rj + 1 < least_cost ->
      let tcosts = cost_int_node rj + 1
      and tkids = kids_int_node rj []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 198; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [rj];
         factored_vars = [];
         insn =
           Insn
             (fun alloc ->
                [Iformat.JumpF (Iformat.Srcreg (lookup_srcreg alloc rj))])},
      tkids
  | Zop Return when 1 < least_cost ->
      let tcosts = 1
      and tkids = []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 199; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [];
         factored_vars = []; insn = Insn (fun alloc -> [Iformat.RetF])},
      tkids
  | Null when 1 < least_cost ->
      let tcosts = 1
      and tkids = []
      and self_cost = 1 in
      Reduction
        {reduction = Null; ruleno = 200; cost = tcosts;
         cost_with_kids = tcosts - self_cost; defs = []; uses = [];
         factored_vars = []; insn = Insn (fun alloc -> [Iformat.NoopF])},
      tkids

(*
>>
*)

(*
let Block.Block(tran,ctrl) = Translate.translist
  (<:asm< add r0,r1,r2; ret >> [])
;;

let myrule = <:rule< rd <- rm + rn >>;;
let asm =    <:asm< add `rd,`rm,`rn >>;


*)

(*
let Block.Block(hd::tl,_) = Translate.translist
  (<:asm< ldr.w r3,@0[r4,#12]; ret >> [])

let q = Select.select rules hd

let _ = Select.emit q

*)
