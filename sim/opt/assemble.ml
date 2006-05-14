(* Assembly code expander *)

let asm_lexer =
  { Token.func = (Token.lexer_func_of_ocamllex Asmlex.asm_token);
    Token.using = (fun _ -> ());
    Token.removing = (fun _ -> ());
    Token.tparse = (fun _ -> None);
    Token.text = Token.lexer_text }
    
let gram = Grammar.create (asm_lexer);;

let asm = Grammar.Entry.create gram "asm";;

(* let dreg = Grammar.Entry.create gram "dreg";;

let reg = Grammar.Entry.create gram "reg";;

let imm = Grammar.Entry.create gram "imm";; *)

let _loc = Lexing.dummy_pos, Lexing.dummy_pos;;
(* let loc = 0,0 *)

exception NotSupported

let aluop3 =
  let h = Hashtbl.create 32 in
    List.iter (fun entry -> let name,valu = entry in Hashtbl.add h name valu)
      [("lsl", <:expr< Iformat.Lsl >>);
       ("lsr", <:expr< Iformat.Lsr >>);
       ("asr", <:expr< Iformat.Asr >>);
       ("ror", <:expr< Iformat.Ror >>);
       ("and", <:expr< Iformat.And >>);
       ("ior", <:expr< Iformat.Ior >>);
       ("eor", <:expr< Iformat.Eor >>);
       ("bic", <:expr< Iformat.Bic >>);
       ("add", <:expr< Iformat.Add >>);
       ("sub", <:expr< Iformat.Sub >>);
       ("rsb", <:expr< Iformat.Rsb >>);
       ("mul", <:expr< Iformat.Mul >>);
       ("div", <:expr< Iformat.Div >>);
       ("udiv", <:expr< Iformat.Udiv >>);
       ("mod", <:expr< Iformat.Mod >>);
       ("umod", <:expr< Iformat.Umod >>)];
  h

let aluop2 =
  let h = Hashtbl.create 32 in
    List.iter (fun entry -> let name,valu = entry in Hashtbl.add h name valu)
      [("mov", <:expr< Iformat.Mov >>);
       ("not", <:expr< Iformat.Not >>)];
  h

let alufp3 =
  let h = Hashtbl.create 32 in
    List.iter (fun entry -> let name,valu = entry in Hashtbl.add h name valu)
      [("addf", <:expr< Iformat.Addf >>);
       ("subf", <:expr< Iformat.Subf >>);
       ("mulf", <:expr< Iformat.Mulf >>);
       ("divf", <:expr< Iformat.Divf >>)];
  h

let alufp2 =
  let h = Hashtbl.create 32 in
    List.iter (fun entry -> let name,valu = entry in Hashtbl.add h name valu)
      [("movf", <:expr< Iformat.Movf >>);
       ("negf", <:expr< Iformat.Negf >>);
       ("absf", <:expr< Iformat.Absf >>);
       ("sqrf", <:expr< Iformat.Sqrf >>)];
  h

let ldtop =
  let h = Hashtbl.create 32 in
    List.iter (fun entry -> let name,valu = entry in Hashtbl.add h name valu)
      [("ldx", <:expr< Iformat.Ldx >>);
       ("ldy", <:expr< Iformat.Ldy >>);
       ("ldz", <:expr< Iformat.Ldz >>)];
    h

exception BadCondition

let ucmp = function
    "eq" -> <:expr< Iformat.Eq >>
  | "ne" -> <:expr< Iformat.Ne >>
  | "lt" -> <:expr< Iformat.Ltu >>
  | "le" -> <:expr< Iformat.Leu >>
  | "gt" -> <:expr< Iformat.Gtu >>
  | "ge" -> <:expr< Iformat.Geu >>
  | _ -> raise BadCondition

let scmp = function
    "eq" -> <:expr< Iformat.Eq >>
  | "ne" -> <:expr< Iformat.Ne >>
  | "lt" -> <:expr< Iformat.Lt >>
  | "le" -> <:expr< Iformat.Le >>
  | "gt" -> <:expr< Iformat.Gt >>
  | "ge" -> <:expr< Iformat.Ge >>
  | _ -> raise BadCondition

let trpc = function
    "n" -> <:expr< Iformat.OnNonzero >>
  | "z" -> <:expr< Iformat.OnZero >>
  | _ -> raise BadCondition

(*
let _ = Quotation.add "op3" (Quotation.ExAst (expand_op3, expand_op3patt))
*)

EXTEND
  GLOBAL: asm;

  asm:
    [[
      il = instlist -> <:expr< fun alloc -> $il$ >>
    ]];

  instlist:
    [[
      inst = opcode -> <:expr< [$inst$] >>
    | inst = opcode; ";"; il = instlist -> <:expr< [$inst$ :: $il$] >>
    ]];

  opcode:
    [[
       op3 = parse_op3 -> op3
     | op2 = parse_op2 -> op2
     | fp3 = parse_fp3 -> fp3
     | fp2 = parse_fp2 -> fp2
     | mvc = parse_mvc -> mvc
     | ldr = parse_ldr -> ldr
     | str = parse_str -> str
     | ldf = parse_ldf -> ldf
     | stf = parse_stf -> stf
     | cmp = parse_cmp -> cmp
     | bfx = parse_bfx -> bfx
     | ldm = parse_ldm -> ldm
     | stm = parse_stm -> stm
     | lmf = parse_lmf -> lmf
     | smf = parse_smf -> smf
     | trp = parse_trp -> trp
     | cbr = parse_cbr -> cbr
     | cal = parse_cal -> cal
     | jmp = parse_jmp -> jmp
     | ret = parse_ret -> ret
     | ldt = parse_ldt -> ldt
     | nop = parse_nop -> nop
    ]];

  parse_op2:
    [[
       opc = OP2; rd = dreg; ","; rm = reg ->
         let foo = Hashtbl.find aluop2 opc in
         <:expr< Iformat.Alu2F($anti:foo$, $anti:rd$, $anti:rm$) >>
     | opc = OP2; rd = dreg; ","; i = imm ->
         let foo = Hashtbl.find aluop2 opc in
         <:expr< Iformat.Alu2iF($anti:foo$, $anti:rd$, $anti:i$) >>
    ]];

  parse_op3:
    [[
       opc = OP3; rd = dreg; ","; rm = reg; ","; rn = reg ->
         let foo = Hashtbl.find aluop3 opc in
         <:expr< Iformat.Alu3F($anti:foo$, $anti:rd$, $anti:rm$, $anti:rn$) >>
     | opc = OP3; rd = dreg; ","; rm = reg; ","; i = imm ->
         let foo = Hashtbl.find aluop3 opc in
         <:expr< Iformat.Alu3iF($anti:foo$, $anti:rd$, $anti:rm$, $anti:i$) >>
    ]];

  prec:
    [[
      "s" -> <:expr< Iformat.Single >>
    | "d" -> <:expr< Iformat.Double >>
    ]];

  parse_fp2:
    [[
       opc = FP2; "."; p = prec; rd = dfreg; ","; rm = freg ->
         let foo = Hashtbl.find alufp2 opc in
         <:expr< Iformat.Alu2fF($anti:foo$, $p$, $anti:rd$, $anti:rm$) >>
     | opc = FP2; "."; p = prec; rd = dfreg; ","; i = imm ->
         let foo = Hashtbl.find alufp2 opc in
         <:expr< Iformat.Alu2fiF($anti:foo$, $p$, $anti:rd$, $anti:i$) >>
    ]];

  parse_fp3:
    [[
       opc = FP3; "."; p = prec; rd = dfreg; ","; rm = freg; ","; rn = freg ->
         let foo = Hashtbl.find alufp3 opc in
         <:expr< Iformat.Alu3fF($anti:foo$, $p$, $anti:rd$,
                                $anti:rm$, $anti:rn$) >>
     | opc = FP3; "."; p = prec; rd = dfreg; ","; rm = freg; ","; i = imm ->
         let foo = Hashtbl.find alufp3 opc in
         <:expr< Iformat.Alu3fiF($anti:foo$, $p$, $anti:rd$,
                                 $anti:rm$, $anti:i$) >>
    ]];

  parse_mvc:
    [[
      MVC; "."; fl = fullness; h = highness; rd = dreg; ","; i = imm ->
        <:expr< Iformat.MvcF($anti:rd$, $anti:i$, $anti:fl$, Iformat.Replace,
                  $anti:h$) >>
    | MVC; "."; h = highness; rd = dreg; ","; i = imm ->
        <:expr< Iformat.MvcF($anti:rd$, $anti:i$, Iformat.Zeros, Iformat.Merge,
                  $anti:h$) >>
    ]];

  fullness:
    [[
      "e" -> <:expr< Iformat.Zeros >>
    | "f" -> <:expr< Iformat.Ones >>
    ]];
  
  highness:
    [[
      "l" -> <:expr< Iformat.Lo >>
    | "h" -> <:expr< Iformat.Hi >>
    ]];

  parse_ldr:
    [[
      LDR; "."; w = width; "v"; rd = dreg; ",";
      "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.LdrF($anti:rd$, $anti:rb$, $anti:ri$,
                  Iformat.Volatile, $anti:w$) >>
    | LDR; "."; w = width; rd = dreg; ",";
      "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.LdrF($anti:rd$, $anti:rb$, $anti:ri$, 
                  Iformat.Nonvolatile, $anti:w$) >>
    | LDR; "."; w = width; "v"; rd = dreg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.LdriF($anti:rd$, $anti:rb$, $anti:offs$, 
                  Iformat.Volatile, $anti:w$) >>
    | LDR; "."; w = width; rd = dreg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.LdriF($anti:rd$, $anti:rb$, $anti:offs$,
                  Iformat.Nonvolatile, $anti:w$) >>
    ]];

  parse_str:
    [[
      STR; "."; w = width; "v"; rs = reg; ",";
           "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.StrF($anti:rs$, $anti:rb$, $anti:ri$, 
                  Iformat.Volatile, $anti:w$) >>
    | STR; "."; w = width; rs = reg; ",";
           "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.StrF($anti:rs$, $anti:rb$, $anti:ri$, 
                  Iformat.Nonvolatile, $anti:w$) >>
    | STR; "."; w = width; "v"; rs = reg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.StriF($anti:rs$, $anti:rb$, $anti:offs$, 
                  Iformat.Volatile, $anti:w$) >>
    | STR; "."; w = width; rs = reg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.StriF($anti:rs$, $anti:rb$, $anti:offs$, 
                  Iformat.Nonvolatile, $anti:w$) >>
    ]];

  width:
    [[
      "b" -> <:expr< Iformat.Byte >>
    | "h" -> <:expr< Iformat.Halfword >>
    | "w" -> <:expr< Iformat.Word >>
    ]];

  parse_ldf:
    [[
      LDF; "."; p = prec; "v"; rd = dfreg; ",";
           "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.LdfF($anti:rd$, $anti:rb$, $anti:ri$,
                  Iformat.Volatile, $p$) >>
    | LDF; "."; p = prec; rd = dfreg; ",";
           "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.LdfF($anti:rd$, $anti:rb$, $anti:ri$,
                  Iformat.Nonvolatile, $p$) >>
    | LDF; "."; p = prec; "v"; rd = dfreg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.LdfiF($anti:rd$, $anti:rb$, $anti:offs$,
                  Iformat.Volatile, $p$) >>
    | LDF; "."; p = prec; rd = dfreg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.LdfiF($anti:rd$, $anti:rb$, $anti:offs$,
                  Iformat.Nonvolatile, $p$) >>
    ]];

  parse_stf:
    [[
      STF; "."; p = prec; "v"; rd = freg; ",";
           "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.StfF($anti:rd$, $anti:rb$, $anti:ri$,
                  Iformat.Volatile, $p$) >>
    | STF; "."; p = prec; rd = freg; ",";
           "["; rb = reg; ","; ri = reg; "]" ->
        <:expr< Iformat.StfF($anti:rd$, $anti:rb$, $anti:ri$, 
                  Iformat.Nonvolatile, $p$) >>
    | STF; "."; p = prec; "v"; rd = freg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.StfiF($anti:rd$, $anti:rb$, $anti:offs$,
                  Iformat.Volatile, $p$) >>
    | STF; "."; p = prec; rd = freg; ",";
           "["; rb = reg; ","; offs = imm; "]" ->
        <:expr< Iformat.StfiF($anti:rd$, $anti:rb$, $anti:offs$,
                  Iformat.Nonvolatile, $p$) >>
    ]];

  parse_cmp:
    [[
      CMP; "." ; c = cond; rd = dreg; ","; rm = reg; ","; rn = reg ->
        <:expr< Iformat.CmpF($anti:c$, $anti:rd$, $anti:rm$, $anti:rn$) >>
    | CMP; "."; c = cond; rd = dreg; ","; rm = reg; ","; i = imm ->
        <:expr< Iformat.CmpiF($anti:c$, $anti:rd$, $anti:rm$, $anti:i$) >>
    | UCMP; "." ; c = ucond; rd = dreg; ","; rm = reg; ","; rn = reg ->
        <:expr< Iformat.CmpF($anti:c$, $anti:rd$, $anti:rm$, $anti:rn$) >>
    | UCMP; "."; c = ucond; rd = dreg; ","; rm = reg; ","; i = imm ->
        <:expr< Iformat.CmpiF($anti:c$, $anti:rd$, $anti:rm$, $anti:i$) >>
    ]];

  cond:
    [[
      name = COMP -> scmp name
    ]];

  ucond:
    [[
      name = COMP -> ucmp name
    ]];

  tcond:
    [[
      name = TRPC -> trpc name
    ]];
  
  parse_bfx:
    [[
      BFX; rd = dreg; ","; rs = reg; "<"; lo = num; ","; hi = num; ">" ->
        <:expr< Iformat.BfxF($anti:rd$, $anti:rs$, $lo$, $hi$, 
                  Iformat.Zeroext) >>
    | BFX; "."; "s"; rd = dreg; ","; rs = reg;
           "<"; lo = num; ","; hi = num; ">" ->
        <:expr< Iformat.BfxF($anti:rd$, $anti:rs$, $lo$, $hi$,
                  Iformat.Signext) >>
    ]];
  
  num:
    [[
      i = INT -> <:expr< $int:i$ >>
    | var = QUOTEDNUM -> <:expr< get_num $lid:var$ >>
    ]];

  dreg123:
    [[
      ri = dreg; ","; rj = dreg; ","; rk = dreg ->
        <:expr< [$anti:ri$; $anti:rj$; $anti:rk$] >>
    | ri = dreg; ","; rj = dreg ->
        <:expr< [$anti:ri$; $anti:rj$] >>
    | ri = dreg ->
        <:expr< [$anti:ri$] >>
    ]];

  reg123:
    [[
      ri = reg; ","; rj = reg; ","; rk = reg ->
        <:expr< [$anti:ri$; $anti:rj$; $anti:rk$] >>
    | ri = reg; ","; rj = reg ->
        <:expr< [$anti:ri$; $anti:rj$] >>
    | ri = reg ->
        <:expr< [$anti:ri$] >>
    ]];

  dfreg123:
    [[
      ri = dfreg; ","; rj = dfreg; ","; rk = dfreg ->
        <:expr< [$anti:ri$; $anti:rj$; $anti:rk$] >>
    | ri = dfreg; ","; rj = dfreg ->
        <:expr< [$anti:ri$; $anti:rj$] >>
    | ri = dfreg ->
        <:expr< [$anti:ri$] >>
    ]];

  freg123:
    [[
      ri = freg; ","; rj = freg; ","; rk = freg ->
        <:expr< [$anti:ri$; $anti:rj$; $anti:rk$] >>
    | ri = freg; ","; rj = freg ->
        <:expr< [$anti:ri$; $anti:rj$] >>
    | ri = freg ->
        <:expr< [$anti:ri$] >>
    ]];

  updown:
    [[
      "i" -> <:expr< Iformat.IncAfter >>
    | "d" -> <:expr< Iformat.DecBefore >>
    ]];

  writeback:
    [[
      "*" -> <:expr< Iformat.Writeback >>
    | "" -> <:expr< Iformat.Discard >>
    ]];

  parse_ldm:
    [[
      LDM; "."; u = updown; rb = dreg; w = writeback; ","; "[";
        rl = dreg123; "]" ->
        <:expr< Iformat.LdmF($anti:rb$, $anti:rl$, $anti:u$, $anti:w$) >>
    ]];
  
  parse_stm:
    [[
      STM; "."; u = updown; rb = dreg; w = writeback; ","; "[";
        rl = reg123; "]" ->
        <:expr< Iformat.StmF($anti:rb$, $anti:rl$, $anti:u$, $anti:w$) >>
    ]];

  parse_lmf:
    [[
      LDMF; "."; u = updown; rb = dreg; w = writeback; ","; "[";
        rl = dfreg123; "]" ->
        <:expr< Iformat.LdmfF($anti:rb$, $anti:rl$, $anti:u$, $anti:w$) >>
    ]];

  parse_smf:
    [[
      STMF; "."; u = updown; rb = dreg; w = writeback; ","; "[";
        rl = freg123; "]" ->
        <:expr< Iformat.StmfF($anti:rb$, $anti:rl$, $anti:u$, $anti:w$) >>
    ]];

  parse_trp:
    [[
      TRAP; "." ; c = tcond; rc = reg; ","; hndlr = dest ->
        <:expr< Iformat.TrapF($anti:c$, $anti:rc$, $anti:hndlr$) >>
    ]];

  parse_cbr:
    [[
      CBR; rs = reg; ","; tru = dest; ","; fal = dest ->
        <:expr< Iformat.CbrF($anti:rs$, $anti:tru$, $anti:fal$) >>
    ]];
  
  parse_cal:
    [[
      CALL; cblk = dest; ","; rblk = dest ->
        <:expr< Iformat.CallF($anti:cblk$, $anti:rblk$) >>
    ]];
  
  parse_jmp:
    [[
      JUMP; jblk = dest ->
        <:expr< Iformat.JumpF($anti:jblk$) >>
    ]];
  
  parse_ret:
    [[
      RET -> <:expr< Iformat.RetF >>
    ]];

  parse_nop:
    [[
      NOP -> <:expr< Iformat.NoopF >>
    ]];

  parse_ldt:
    [[
      kind = LDT; imm = ldtimm ->
        let foo = Hashtbl.find ldtop kind in
        <:expr< Iformat.LdtF($anti:foo$, $anti:imm$) >>
    ]];

  ldtimm:
    [[
      imm = imm -> <:expr< $anti:imm$ >>
    | var = QUOTEDLOBITS ->
        <:expr< split_lobits $lid:var$ >>
    ]];

  dest:
    [[
      r = reg -> <:expr< Iformat.Srcreg($anti:r$) >>
    | i = imm -> <:expr< Iformat.Bits(Int32.to_int $anti:i$) >>
    | var = QUOTEDHIBITS ->
        <:expr< Iformat.Bits(split_hibits $lid:var$) >>
    ]];

  dreg:
    [[
      "r"; num = INT ->
        <:expr< Iformat.Dreg($int:num$) >>
    | var = QUOTEDREG ->
        let regnam = "r" ^ var in
        <:expr< lookup_destreg alloc $lid:regnam$ >>
    ]];

  dfreg:
    [[
      "f"; num = INT ->
        <:expr< Iformat.Dfreg($int:num$) >>
    | var = QUOTEDFREG ->
        let regnam = "f" ^ var in
        <:expr< lookup_destfreg alloc $lid:regnam$ >>
    ]];

(*    | dreg = ANTI_DREG ->
        <:expr< Iformat.Dreg($int:dreg$) >> *)

  reg:
    [[
      "r"; num = INT ->
        <:expr< Iformat.Reg($int:num$, Iformat.Alive) >>
    | "~"; "r"; num = INT ->
        <:expr< Iformat.Reg($int:num$, Iformat.Dead) >>
    | var = QUOTEDREG ->
        let regnam = "r" ^ var in
        <:expr< lookup_srcreg alloc $lid:regnam$ >>
    ]];

  freg:
    [[
      "f"; num = INT ->
        <:expr< Iformat.Freg($int:num$, Iformat.Alive) >>
    | "~"; "f"; num = INT ->
        <:expr< Iformat.Freg($int:num$, Iformat.Dead) >>
    | var = QUOTEDFREG ->
        let regnam = "f" ^ var in
        <:expr< lookup_srcfreg alloc $lid:regnam$ >>
    ]];

  imm:
    [[
      "#"; num = INT ->
        <:expr< Int32.of_string ($str:num$) >>
    | var = QUOTEDIMM ->
        <:expr< get_const $lid:var$ >>
    ]];
END
;;

let add_quot_asm rule name =
  let exp s = Grammar.Entry.parse rule (Stream.of_string s) in
  let pat s = failwith "pat type not implemented" in
    Quotation.add name (Quotation.ExAst (exp, pat))

let _ = add_quot_asm asm "asm"

(*
let _ = Quotation.default := "asm"
;;
*)
