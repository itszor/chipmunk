(* Match fragments of abstract syntax tree *)

open Ast
open Memotree

type env = (string, ast) Hashtbl.t

type ccr = {
  m_cost : int;
  m_children : memo list;
  m_reduction : ast matchtype;
  m_ast : ast;
}

type matcher = env -> Ast.ast -> Memotree.memotree -> ccr -> ccr

(* type mfun = (ast -> bool)

and mtree = MBinop of mfun * mtree * mtree
          | MUnop of mfun * mtree
          | MMove of mtree * mtree
          | MProduction of mtree * mtree
          | MVoid of mtree
          | MLeaf of mfun * string
          | MPair of mfun * string * string *)

let makemakepseudo typ =
  let makepseudo =
    let num = ref 0 in
    fun ast ->
      num := !num+1;
      Pseudo(!num, typ, Use)
  in
    makepseudo

let newpseudo = makemakepseudo Id.IntType

exception Fail

let rec nonmatched = function
    MMatch(_,x) -> nonmatched x
  | x -> x

let rec intcost = function
    MMatch(memo,rest) as m ->
      begin match memo.reduce with
        IntNode(q) -> q,m
      | _ -> intcost rest
      end
  | _ -> raise Fail

let rec fltcost = function
    MMatch(memo,rest) as m ->
      begin match memo.reduce with
        FloatNode(q) -> q,m
      | _ -> intcost rest
      end
  | _ -> raise Fail

let rec denoted = function
    Note(n,nast) -> denoted nast
  | x -> x

let rec renote astwithnotes newast =
  match astwithnotes with
    Note(n, othernotes) -> Note(n, renote othernotes newast)
  | _ -> newast

(* Match a register / integer pseudo of int type or ptr type *)
(* let match_reg name env ast memo ccr =
  match denoted ast with
    Register(_, Id.IntType, _)
  | Register(_, Id.PtrType, _)
  | Pseudo(_, Id.IntType, _)
  | Pseudo(_, Id.PtrType, _) -> Hashtbl.add env name ast; {ccr with m_ast=ast}
  | _ ->
    begin match intcost memo with
      red,MMatch(m,_) ->
        Hashtbl.add env name red;
        {ccr with m_cost = ccr.m_cost + m.cost;
                  m_children = m :: ccr.m_children;
                  m_ast = red}
    | _ -> raise Fail
    end

let match_freg name env ast memo ccr =
  match denoted ast with
    Register(_, Id.FloatType, _)
  | Pseudo(_, Id.FloatType, _) -> Hashtbl.add env name ast; {ccr with m_ast=ast}
  | _ ->
    begin match fltcost memo with
      red,MMatch(m,_) ->
        Hashtbl.add env name red;
        {ccr with m_cost = ccr.m_cost + m.cost;
                  m_children = m :: ccr.m_children;
                  m_ast = red}
    | _ -> raise Fail
    end *)

(* Match a constant *)
(* let match_const const env ast memo ccr =
  match denoted ast with
    Constant(astconst) when const=astconst -> {ccr with m_ast=ast}
  | _ -> raise Fail *)

(* Match immediates valid for shift instruction *)
(* let match_shiftimm name env ast memo cost =
  match denoted ast with
    Constant(astconst) when astconst >= Int32.zero &&
                            astconst <= (Int32.of_int 31) ->
      Hashtbl.add env name ast;
      {cost with m_ast = ast}
  | _ -> raise Fail *)

let match_shiftimm = function
    Constant(c) -> c >= 0l && c <= 31l
  | _ -> failwith "Not a constant"

(* Match immediates valid for data processing instruction *)
(*let match_dataimm name env ast memo cost =
  match denoted ast with
    Constant(a) when
        (Int32.logand a 0xff000000l) = a ||
        (Int32.logand a 0x00ff0000l) = a ||
        (Int32.logand a 0x0000ff00l) = a ||
        (Int32.logand a 0x000000ffl) = a ->
      Hashtbl.add env name ast;
      {cost with m_ast = ast}
  | _ -> raise Fail *)

let match_dataimm = function
    Constant(c) ->
      (Int32.logand c 0xff000000l) = c ||
      (Int32.logand c 0x00ff0000l) = c ||
      (Int32.logand c 0x0000ff00l) = c ||
      (Int32.logand c 0x000000ffl) = c
  | _ -> failwith "Not a constant"

let match_negdataimm = function
    Constant(c) ->
      let nc = Int32.neg c in
      (Int32.logand nc 0xff000000l) = nc ||
      (Int32.logand nc 0x00ff0000l) = nc ||
      (Int32.logand nc 0x0000ff00l) = nc ||
      (Int32.logand nc 0x000000ffl) = nc
  | _ -> failwith "Not a negative data constant"

let match_maybenegdataimm c = match_negdataimm c || match_dataimm c

let negate_ast_const = function
    Constant(c) -> Constant(Int32.neg c)
  | _ -> failwith "Not constant"

(* Match immediates suitable for floating-point instruction.
   (These are the same, but stored as a float!)
*)
(*let match_floatimm name env ast memo cost =
  match denoted ast with
    Floatconst(a) ->
      let q = Int32.of_float a in
      let w = Int32.to_float q in
        if (w = a) && ((Int32.logand q 0xff000000l) = q ||
                       (Int32.logand q 0x00ff0000l) = q ||
                       (Int32.logand q 0x0000ff00l) = q ||
                       (Int32.logand q 0x000000ffl) = q)
        then begin
          Hashtbl.add env name ast;
          {cost with m_ast=ast}
        end else
          raise Fail
  | _ -> raise Fail*)

let match_floatimm = function
    Floatconst(a) ->
      let q = Int32.of_float a in
      let w = Int32.to_float q in
      (w = a) && ((Int32.logand q 0xff000000l) = q ||
                  (Int32.logand q 0x00ff0000l) = q ||
                  (Int32.logand q 0x0000ff00l) = q ||
                  (Int32.logand q 0x000000ffl) = q)
  | _ -> failwith "Not a float constant"

(* Match negated immediates valid for data processing instruction *)
(*let match_invdataimm name env ast memo cost =
  match denoted ast with
    Constant(a) when
      let na = Int32.lognot a in
        (Int32.logand na 0xff000000l) = na ||
        (Int32.logand na 0x00ff0000l) = na ||
        (Int32.logand na 0x0000ff00l) = na ||
        (Int32.logand na 0x000000ffl) = na ->
      Hashtbl.add env name (Constant(Int32.lognot a));
      {cost with m_ast=ast}
  | _ -> raise Fail*)

let match_invdataimm = function
    Constant(a) ->
      let na = Int32.lognot a in
        (Int32.logand na 0xff000000l) = na ||
        (Int32.logand na 0x00ff0000l) = na ||
        (Int32.logand na 0x0000ff00l) = na ||
        (Int32.logand na 0x000000ffl) = na
  | _ -> failwith "Not a constant"

(* Match immediates suitable for mvc.eh instruction *)
(*let match_ehimm name env ast memo cost =
  match denoted ast with
    Constant(a) when (Int32.logand a 0xffff0000l) = a ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_ehimm = function
    Constant(a) -> (Int32.logand a 0xffff0000l) = a
  | _ -> failwith "Not a constant"

(* Match immediates suitable for mvc.el instruction *)
(* let match_elimm name env ast memo cost =
  match denoted ast with
    Constant(a) when (Int32.logand a 0x0000ffffl) = a ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_elimm = function
    Constant(a) -> (Int32.logand a 0x0000ffffl) = a
  | _ -> failwith "Not a constant"

(* Match immediates suitable for mvc.fh instruction *)
(*let match_fhimm name env ast memo cost =
  match denoted ast with
    Constant(a) when
        (Int32.logor 0x0000ffffl
                     (Int32.logand a 0xffff0000l)) = a ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_fhimm = function
    Constant(a) -> (Int32.logor 0x0000ffffl
                                (Int32.logand a 0xffff0000l)) = a
  | _ -> failwith "Not a constant"

(* Match immediates suitable for mvc.fl instruction *)
(* let match_flimm name env ast memo cost =
  match denoted ast with
    Constant(a) when
        (Int32.logor 0xffff0000l
                     (Int32.logand a 0x0000ffffl)) = a ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_flimm = function
    Constant(a) -> (Int32.logor 0xffff0000l
                                (Int32.logand a 0x0000ffffl)) = a
  | _ -> failwith "Not a constant"

(* Match immediates suitable for word-memory offset *)
(* let match_wmimm name env ast memo cost =
  match denoted ast with
    Constant(astconst) when
        astconst >= (Int32.of_int (-256)) &&
        astconst < (Int32.of_int 256) &&
        ((Int32.logand astconst (Int32.of_int 3)) = Int32.zero) ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_wmimm = function
    Constant(c) ->
      c >= -512l &&
      c < 512l &&
      ((Int32.logand c 3l) = 0l)
  | _ -> failwith "Not a constant"

(* Match immediates suitable for halfword-memory offset *)
(* let match_hmimm name env ast memo cost =
  match denoted ast with
    Constant(astconst) when
        astconst >= (Int32.of_int (-128)) &&
        astconst < (Int32.of_int 128) &&
        (Int32.logand astconst (Int32.of_int 1)) = Int32.zero ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_hmimm = function
    Constant(c) ->
      c >= -256l &&
      c < 256l &&
      ((Int32.logand c 1l) = 0l)
  | _ -> failwith "Not a constant"

(* Match immediates suitable for byte-memory offset *)
(* let match_bmimm name env ast memo cost =
  match denoted ast with
    Constant(astconst) when
        astconst >= (Int32.of_int (-64)) &&
        astconst < (Int32.of_int 64) ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

let match_bmimm = function
    Constant(c) -> c >= -128l && c < 128l
  | _ -> failwith "Not a constant"

(* let match_imm name env ast memo cost =
  match denoted ast with
    Constant _ ->
      Hashtbl.add env name ast;
      {cost with m_ast=ast}
  | _ -> raise Fail *)

(* let match_triop opcode op1 op2 op3 env ast memo cost =
  match denoted ast,nonmatched memo with
    Triop(opcode',op1',op2',op3'),MTri(m1,m2,m3) ->
      if opcode = opcode' then
        let cost1 = op1 env op1' m1 cost in
        let cost2 = op2 env op2' m2 cost1 in
        let cost3 = op3 env op3' m3 cost2 in
        {cost3 with m_ast=
          renote ast (Triop(opcode', cost1.m_ast, cost2.m_ast, cost3.m_ast))}
      else
        raise Fail
  | _ -> raise Fail

(* Match a particular binary operator *)
let match_binop opcode op1 op2 env ast memo cost =
  match denoted ast,nonmatched memo with
    Binop(opcode',op1',op2'),MBin(m1,m2) ->
      if opcode = opcode' then
        let cost1 = op1 env op1' m1 cost in
        let cost2 = op2 env op2' m2 cost1 in
        {cost2 with m_ast=
          renote ast (Binop(opcode', cost1.m_ast, cost2.m_ast))}
      else
        raise Fail
  | _ -> raise Fail

(* Match a particular unary operator *)
let match_unop opcode op1 env ast memo cost =
  match denoted ast,nonmatched memo with
    Unop(opcode',op1'),MUn(m1) ->
      if opcode = opcode' then
        let cost1 = op1 env op1' m1 cost in
        {cost1 with m_ast=
          renote ast (Unop(opcode', cost1.m_ast))}
      else
        raise Fail
  | _ -> raise Fail

let match_nop opcode env ast memo cost =
  match denoted ast, nonmatched memo with
    Nop(opcode'), MNull ->
      if opcode = opcode' then
        {cost with m_ast=ast}
      else
        raise Fail
  | _ -> raise Fail

let match_ind width domname child env ast memo cost =
  match denoted ast,nonmatched memo with
    Unop(Ind(vol',width'),child'),MUn(m1) ->
      if width = width' then begin
        Hashtbl.add env domname (MatchVol vol');
        let cost1 = child env child' m1 cost in
        {cost1 with m_ast=
          renote ast (Unop(Ind(vol',width'), cost1.m_ast))}
      end else
        raise Fail
  | _ -> raise Fail

let match_bitpair startname endname env ast memo cost =
  match denoted ast,nonmatched memo with
    Pair(st', en'),MBin(m1,m2) ->
      let cost1 = match_shiftimm startname env st' m1 cost in
      let cost2 = match_shiftimm endname env en' m2 cost1 in
      {cost2 with m_ast=
        renote ast (Pair(cost1.m_ast, cost2.m_ast))}
  | _ -> raise Fail

exception EmptyList

let getstartend ast =
  let rec findend = function
    [e] -> e
  | f::fs -> findend fs
  | _ -> raise EmptyList
  in
  match ast with
    [singleton] -> singleton,singleton
  | foo::foos -> foo,(findend foos)
  | _ -> raise EmptyList

(* Matches monotonic increasing/decreasing register list.
 * Does not match empty register list, since that isn't defined for
 * our ldm/stm instructions
 *)
let match_vector startname endname env ast memo cost =
  let rec seqlist listitems lastreg difference =
    match listitems with
      [] -> true
    | Register((thisreg,loc),_,expire)::rs ->
        begin match difference with
          None -> (abs (thisreg-lastreg))=1
                  && seqlist rs thisreg (Some (thisreg-lastreg))
        | Some diff ->
            if (thisreg-lastreg)=diff then
              seqlist rs thisreg (Some (thisreg-lastreg))
            else false
        end
    | _ -> false
  in match denoted ast with
    Vector(_,reglist) ->
      begin match reglist with
        Register((num,loc),_,expire)::rs ->
          if seqlist rs num None then begin
            let start,finish = getstartend reglist in
            Hashtbl.add env startname start;
            Hashtbl.add env endname finish;
            {cost with m_ast=ast}
          end else
            raise Fail
      | _ -> raise Fail
      end
  | _ -> raise Fail

(* Same as above, but for a float vector
 *)
(* let match_vectorf startname endname env ast memo cost =
  let rec seqlist listitems lastreg difference =
    match listitems with
      [] -> true
    | Floatreg((thisreg,loc),expire)::rs ->
        begin match difference with
          None -> (abs (thisreg-lastreg))=1
                  && seqlist rs thisreg (Some (thisreg-lastreg))
        | Some diff ->
            if (thisreg-lastreg)=diff then
              seqlist rs thisreg (Some (thisreg-lastreg))
            else false
        end
    | _ -> false
  in match denoted ast with
    Vector(_,reglist) ->
      begin match reglist with
        Floatreg((num,loc),expire)::rs ->
          if seqlist rs num None then begin
            let start,finish = getstartend reglist in
            Hashtbl.add env startname start;
            Hashtbl.add env endname finish;
            {cost with m_ast=ast}
          end else
            raise Fail
      | _ -> raise Fail
      end
  | _ -> raise Fail *)

(* Difficult to not match a prod, like *)
(* Needs to generate different type of pseudo for different types of lhs *)
(*
let match_prod lhs rhs env ast =
  match ast with
    Move(lhs',rhs') ->
      Move(lhs env lhs', rhs env rhs')
  | other ->
      let pseu = newpseudo (rhs env other) in
      ignore (lhs env pseu);
      pseu
*)

let match_move lhs rhs env ast memo cost =
  match denoted ast,nonmatched memo with
    Move(lhs',rhs'),MBin(ml,mr) ->
      let cost1 = lhs env lhs' ml cost in
      let cost2 = rhs env rhs' mr cost1 in
      {cost2 with m_ast=
        renote ast (Move(cost1.m_ast, cost2.m_ast))}
  | _ -> raise Fail

let match_reduction lhs rhs env ast memo ccr =
  let cost' = rhs env ast memo ccr in
  let reduce = lhs env in
  {cost' with m_reduction = IntNode reduce;
              m_ast = renote ast (Move(reduce, cost'.m_ast))}

let make_temp name env =
  let pseu = newpseudo () in
  Hashtbl.add env name pseu;
  pseu
*)
