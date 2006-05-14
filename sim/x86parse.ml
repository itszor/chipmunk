open Seq
open X86support

#load "q_MLast.cmo";;
#load "pa_extend.cmo";;

let x86_lexer =
  { Token.func = (Token.lexer_func_of_ocamllex X86tok.x86_token);
    Token.using = (fun _ -> ());
    Token.removing = (fun _ -> ());
    Token.tparse = (fun _ -> None);
    Token.text = Token.lexer_text }

let gram = Grammar.create (x86_lexer);;

let x86 = Grammar.Entry.create gram "x86";;

let loc = 0,0;;

exception BadFormat

let eax_imm32_ops = function
    "add" -> 0x05
  | "or" -> 0x0d
  | "adc" -> 0x15
  | "sbb" -> 0x1d
  | "and" -> 0x25
  | "sub" -> 0x2d
  | "xor" -> 0x35
  | "cmp" -> 0x3d
  | "test" -> 0xa9
  | _ -> raise BadFormat

let rm32_imm32_ops = function
    "add" -> 0x81,0
  | "or" -> 0x81,1
  | "adc" -> 0x81,2
  | "sbb" -> 0x81,3
  | "and" -> 0x81,4
  | "sub" -> 0x81,5
  | "xor" -> 0x81,6
  | "cmp" -> 0x81,7
  | "mov" -> 0xc7,0
  | "test" -> 0xf7,0
  | _ -> raise BadFormat

let rm32_imm8_ops = function
    "add" -> None,0x83,0
  | "or" -> None,0x83,1
  | "adc" -> None,0x83,2
  | "sbb" -> None,0x83,3
  | "and" -> None,0x83,4
  | "sub" -> None,0x83,5
  | "xor" -> None,0x83,6
  | "cmp" -> None,0x83,7
  | "bt" -> Some 0x0f,0xba,4
  | "bts" -> Some 0x0f,0xba,5
  | "btr" -> Some 0x0f,0xba,6
  | "btc" -> Some 0x0f,0xba,7
  | "rol" -> None,0xc1,0
  | "ror" -> None,0xc1,1
  | "rcl" -> None,0xc1,2
  | "rcr" -> None,0xc1,3
  | "shl" -> None,0xc1,4
  | "shr" -> None,0xc1,5
  | "sar" -> None,0xc1,7
  | _ -> raise BadFormat

let reg32_rm32_ops = function
    "add" -> None,0x03
  | "or" -> None,0x0b
  | "adc" -> None,0x13
  | "sbb" -> None,0x1b
  | "and" -> None,0x23
  | "sub" -> None,0x2b
  | "xor" -> None,0x33
  | "cmp" -> None,0x3b
  | "xchg" -> None,0x87
  | "mov" -> None,0x8b
  | "imul" -> Some 0x0f,0xaf
  | "bsf" -> Some 0x0f,0xbc
  | "bsr" -> Some 0x0f,0xbd
  | "lea" -> None,0x8d
  | _ -> raise BadFormat

let rm32_reg32_ops = function
    "add" -> None,0x01
  | "or" -> None,0x09
  | "adc" -> None,0x11
  | "sbb" -> None,0x19
  | "and" -> None,0x21
  | "sub" -> None,0x29
  | "xor" -> None,0x31
  | "cmp" -> None,0x39
  | "bt" -> Some 0x0f,0xa3
  | "bts" -> Some 0x0f,0xab
  | "btr" -> Some 0x0f,0xb3
  | "btc" -> Some 0x0f,0xbb
  | "test" -> None,0x85
  | "mov" -> None,0x89
  | "xadd" -> Some 0x0f,0xc1
  | _ -> raise BadFormat

let rm32_ops = function
    "pop" -> 0x8f,0
  | "rol" -> 0xd1,0
  | "ror" -> 0xd1,1
  | "rcl" -> 0xd1,2
  | "rcr" -> 0xd1,3
  | "shl" -> 0xd1,4
  | "shr" -> 0xd1,5
  | "sar" -> 0xd1,7
  | "not" -> 0xf7,2
  | "neg" -> 0xf7,3
  | "mul" -> 0xf7,4
  | "imul" -> 0xf7,5
  | "div" -> 0xf7,6
  | "idiv" -> 0xf7,7
  | "inc" -> 0xff,0
  | "dec" -> 0xff,1
  | "call" -> 0xff,2
  | "jmp" -> 0xff,4
  | "push" -> 0xff,6
  | _ -> raise BadFormat

let rm8_cl_ops = function
    "rol" -> 0xd2,0
  | "ror" -> 0xd2,1
  | "rcl" -> 0xd2,2
  | "rcr" -> 0xd2,3
  | "shl" -> 0xd2,4
  | "shr" -> 0xd2,5
  | "sar" -> 0xd2,7
  | _ -> raise BadFormat

let rm32_cl_ops = function
    "rol" -> 0xd3,0
  | "ror" -> 0xd3,1
  | "rcl" -> 0xd3,2
  | "rcr" -> 0xd3,3
  | "shl" -> 0xd3,4
  | "shr" -> 0xd3,5
  | "sar" -> 0xd3,7
  | _ -> raise BadFormat

let reg32_ops r = function
    "inc" -> None,0x40+r
  | "dec" -> None,0x48+r
  | "push" -> None,0x50+r
  | "pop" -> None,0x58+r
  | "xchg" -> None,0x90+r
  | "bswap" -> Some 0x0f,0xc8+r
  | _ -> raise BadFormat

let narg_ops = function
    "pusha" -> 0x60
  | "popa" -> 0x61
  | "outsb" -> 0x6e
  | "outs" -> 0x6f
  | "nop" -> 0x90
  | "pushf" -> 0x9c
  | "popf" -> 0x9d
  | "sahf" -> 0x9e
  | "lahf" -> 0x9f
  | "movsb" -> 0xa4
  | "movs" -> 0xa5
  | "stosb" -> 0xaa
  | "stos" -> 0xab
  | "scasb" -> 0xae
  | "scas" -> 0xaf
  | "ret" -> 0xc3
  | "leave" -> 0xc9
  | "retf" -> 0xcb
  | "lock" -> 0xf0
  | "cmc" -> 0xf5
  | "clc" -> 0xf8
  | "stc" -> 0xf9
  | "cli" -> 0xfa
  | "sti" -> 0xfb
  | "cld" -> 0xfc
  | "std" -> 0xfd
  | _ -> raise BadFormat

let al_imm8_ops = function
    "add" -> 0x04
  | "or" -> 0x0c
  | "adc" -> 0x14
  | "sbb" -> 0x1c
  | "and" -> 0x24
  | "sub" -> 0x2c
  | "xor" -> 0x34
  | "cmp" -> 0x3c
  | "test" -> 0xa8
  | _ -> raise BadFormat

let rm8_imm8_ops = function
    "add" -> 0x80,0
  | "or" -> 0x80,1
  | "adc" -> 0x80,2
  | "sbb" -> 0x80,3
  | "and" -> 0x80,4
  | "sub" -> 0x80,5
  | "xor" -> 0x80,6
  | "cmp" -> 0x80,7
  | "rol" -> 0xc0,0
  | "ror" -> 0xc0,1
  | "rcl" -> 0xc0,2
  | "rcr" -> 0xc0,3
  | "shl" -> 0xc0,4
  | "shr" -> 0xc0,5
  | "sar" -> 0xc0,7
  | "mov" -> 0xc6,0
  | "test" -> 0xf6,0
  | _ -> raise BadFormat

let rm8_reg8_ops = function
    "add" -> None,0x00
  | "or" -> None,0x08
  | "adc" -> None,0x10
  | "sbb" -> None,0x18
  | "and" -> None,0x20
  | "sub" -> None,0x28
  | "xor" -> None,0x30
  | "cmp" -> None,0x38
  | "test" -> None,0x84
  | "mov" -> None,0x88
  | "xadd" -> Some 0x0f,0xc0
  | _ -> raise BadFormat

let reg8_rm8_ops = function
    "add" -> 0x02
  | "or" -> 0x0a
  | "adc" -> 0x12
  | "sbb" -> 0x1a
  | "and" -> 0x22
  | "sub" -> 0x2a
  | "xor" -> 0x32
  | "cmp" -> 0x3a
  | "xchg" -> 0x86
  | "mov" -> 0x8a
  | _ -> raise BadFormat

let rm8_ops = function
    "seto" -> Some 0x0f,0x90,0
  | "setno" -> Some 0x0f,0x91,0
  | "setb" -> Some 0x0f,0x92,0
  | "setae" -> Some 0x0f,0x93,0
  | "sete" -> Some 0x0f,0x94,0
  | "setne" -> Some 0x0f,0x95,0
  | "setbe" -> Some 0x0f,0x96,0
  | "seta" -> Some 0x0f,0x97,0
  | "sets" -> Some 0x0f,0x98,0
  | "setns" -> Some 0x0f,0x99,0
  | "setpe" -> Some 0x0f,0x9a,0
  | "setpo" -> Some 0x0f,0x9b,0
  | "setl" -> Some 0x0f,0x9c,0
  | "setge" -> Some 0x0f,0x9d,0
  | "setle" -> Some 0x0f,0x9e,0
  | "setg" -> Some 0x0f,0x9f,0
  | "rol" -> None,0xd0,0
  | "ror" -> None,0xd0,1
  | "rcl" -> None,0xd0,2
  | "rcr" -> None,0xd0,3
  | "shl" -> None,0xd0,4
  | "shr" -> None,0xd0,5
  | "sar" -> None,0xd0,7
  | "inc" -> None,0xfe,0
  | "dec" -> None,0xfe,1
  | "not" -> None,0xf6,2
  | "neg" -> None,0xf6,3
  | "mul" -> None,0xf6,4
  | "imul" -> None,0xf6,5
  | "div" -> None,0xf6,6
  | "idiv" -> None,0xf6,7
  | _ -> raise BadFormat

let jump_near_ops = function
    "jo" -> 0x70
  | "jno" -> 0x71
  | "jb" -> 0x72
  | "jae" -> 0x73
  | "je" -> 0x74
  | "jne" -> 0x75
  | "jbe" -> 0x76
  | "ja" -> 0x77
  | "js" -> 0x78
  | "jns" -> 0x79
  | "jpe" -> 0x7a
  | "jpo" -> 0x7b
  | "jl" -> 0x7c
  | "jge" -> 0x7d
  | "jle" -> 0x7e
  | "jg" -> 0x7f
  | "jecxz" -> 0xe3
  | "jmp" -> 0xeb
  | _ -> raise BadFormat

let jump_far_ops = function
    "jo" -> Some 0x0f,0x80
  | "jno" -> Some 0x0f,0x81
  | "jb" -> Some 0x0f,0x82
  | "jae" -> Some 0x0f,0x83
  | "je" -> Some 0x0f,0x84
  | "jne" -> Some 0x0f,0x85
  | "jbe" -> Some 0x0f,0x86
  | "ja" -> Some 0x0f,0x87
  | "js" -> Some 0x0f,0x88
  | "jns" -> Some 0x0f,0x89
  | "jpe" -> Some 0x0f,0x8a
  | "jpo" -> Some 0x0f,0x8b
  | "jl" -> Some 0x0f,0x8c
  | "jge" -> Some 0x0f,0x8d
  | "jle" -> Some 0x0f,0x8e
  | "jg" -> Some 0x0f,0x8f
  | "call" -> None,0xe8
  | "jmp" -> None,0xe9
  | _ -> raise BadFormat

let extend_byte_ops = function
    "movsx" -> Some 0x0f,0xbe
  | "movzx" -> Some 0x0f,0xb6
  | _ -> raise BadFormat

let extend_word_ops = function
    "movsx" -> Some 0x0f,0xbf
  | "movzx" -> Some 0x0f,0xb7
  | _ -> raise BadFormat

let imm8_ops = function
    "push" -> 0x6a
  | _ -> raise BadFormat

let imm32_ops = function
    "push" -> 0x68
  | _ -> raise BadFormat

let rm32_reg32_imm8_ops = function
    "shld" -> Some 0x0f,0xa4
  | "shrd" -> Some 0x0f,0xac
  | _ -> raise BadFormat

let rm32_reg32_cl_ops = function
    "shld" -> Some 0x0f,0xa5
  | "shrd" -> Some 0x0f,0xad
  | _ -> raise BadFormat

let reg32_rm32_imm8_ops = function
    "imul" -> 0x6b
  | _ -> raise BadFormat

let reg32_rm32_imm32_ops = function
    "imul" -> 0x69
  | _ -> raise BadFormat

let split_imm32 n =
  let nn = Int32.of_string n in List.map Int32.to_string (split_int32 nn)

let modrm mode rop rm =
  let modbit = match mode with
    `indirect -> 0*64
  | `disp8 -> 1*64
  | `disp32 -> 2*64
  | `direct -> 3*64
  and ropbit = match rop with
    `subcode x -> x*8
  | `reg x -> x*8
  and rmbit = match rm with
    `reg x -> x
  | `sib -> 4
  in
    ropbit lor modbit lor rmbit

let scindbase scale index base =
  let scalecode = match scale with
    1 -> 0
  | 2 -> 1
  | 4 -> 2
  | 8 -> 3
  | _ -> raise BadFormat
  in base lor (index*8) lor (scalecode*64)

let makelist el =
  List.fold_right (fun x l -> <:expr< [$x$ :: $l$] >>) el <:expr< [] >>

let makeintlist el =
  makelist
    (List.map (fun n -> let m = string_of_int n in <:expr< $int:m$ >>) el)

let addrmode ~sub:sub ~reg:reg ~offset:offset =
  let mode = match offset with
    0l -> `indirect
  | x ->
      if x >= -128l && x < 128l then
        `disp8
      else
        `disp32
  in let modrmsib = match reg with
    `reg x as r -> [modrm `direct sub r]
  | `index (s,i,b) -> [modrm mode sub `sib; scindbase s i b]
  | `indirect b ->
      begin match b with
        4 -> [modrm mode sub `sib; scindbase 1 4 4]
      | _ -> [modrm mode sub (`reg b)]
      end
  in
  let bytelist =
    match reg with
      `reg _ -> modrmsib
    | _ ->
      begin match mode with
      | `indirect -> modrmsib
      | `disp8 -> modrmsib @ [Int32.to_int offset]
      | `disp32 -> modrmsib @ (List.map Int32.to_int (split_int32 offset))
      end
  in
    let x = makeintlist bytelist in
      <:expr< Bytes $anti:x$ >>

(* This is a bit nasty. *)
let rec stringify = function
    [] -> ""
  | l::ls -> let q = String.create 1 in q.[0] <- char_of_int l; q ^ stringify ls

let prefstr = function
    None -> <:expr< Bytes [] >>
  | Some x -> let ix = string_of_int x in <:expr< Bytes [$int:ix$] >>

EXTEND
  GLOBAL: x86;
  
  x86:
    [[
      aa = itemlist -> aa
    ]];
  
  itemlist:
    [[
      EOI -> <:expr< Empty >>
    | i = item -> <:expr< $i$ >>
    | i = item; ";"; is = itemlist -> <:expr< lazy_append $i$ $is$ >>
    ]];
  
  (*width:
    [[
      w = WIDTH ->
        match w with
          "byte" -> `byte
        | "dword" -> `dword
        | _ -> raise BadFormat
    ]]; *)
      
  rm32:
    [[
     rnum = reg32 ->
      addrmode ~reg:(`reg rnum) ~offset:0l
    | "["; reg = reg32; "]" ->
      let rnum = reg in
        addrmode ~reg:(`indirect rnum) ~offset:0l
    | "["; base = reg32; "+"; index = reg32; "]" ->
      addrmode ~reg:(`index (1, base, index))
               ~offset:0l
    | "["; reg = reg32; "+"; n = NUM; "]" ->
      addrmode ~reg:(`indirect (reg))
               ~offset:(Int32.of_string n)
    | "["; base = reg32; "+"; index = reg32; "+"; n = NUM; "]" ->
      addrmode ~reg:(`index (1, base, index))
               ~offset:(Int32.of_string n)
    | "["; base = reg32; "+"; scale = NUM; "*"; index = reg32; "]" ->
      addrmode ~reg:(`index (int_of_string scale,
                             base,
                             index))
               ~offset:0l
    | "["; base = reg32; "+"; scale = NUM; "*"; index = reg32; "+"; n = NUM;
      "]" ->
      addrmode ~reg:(`index (int_of_string scale,
                             base,
                             index))
               ~offset:(Int32.of_string n)
    ]];

  reg8:
    [[
      "al" -> 0
    | "cl" -> 1
    | "dl" -> 2
    | "bl" -> 3
    | "ah" -> 4
    | "ch" -> 5
    | "dh" -> 6
    | "bh" -> 7
    ]];
  
  reg8_al_only:
    [[
      "al" -> 0
    ]];
  
  reg8_cl_only:
    [[
      "cl" -> 1
    ]];

  reg32:
    [[
      "eax" -> 0
    | "ecx" -> 1
    | "edx" -> 2
    | "ebx" -> 3
    | "esp" -> 4
    | "ebp" -> 5
    | "esi" -> 6
    | "edi" -> 7
    ]];

  item:
    [[
      i = insn -> i
    | l = label -> l
    ]];

  label:
    [[
      LABEL; arg = ID ->
        <:expr< lazy_cons (Label $str:arg$) Empty >>
    ]];

  insn:
    [[
      (* Not all al,#n ops have a short form, so degrade to the long
         form if necessary
      *)
      op = OP; reg = reg8_al_only; ","; n = num8 ->
        begin try
          let opbyte = string_of_int (al_imm8_ops op) in
            <:expr< lazy_cons (Byte $int:opbyte$)
                      (lazy_cons $n$ Empty) >>
        with BadFormat ->
          let opbyte,sub = rm8_imm8_ops op in
          let rmfit = string_of_int
            (modrm `direct (`subcode sub) (`reg reg)) in
          let opbstr = string_of_int opbyte in
            <:expr< lazy_cons (Byte $int:opbstr$)
                      (lazy_cons (Byte $int:rmfit$)
                        (lazy_cons $n$ Empty)) >>
        end

    | op = OP; reg = reg8_al_only; ","; "byte"; rm = rm32 ->
        let opbyte = reg8_rm8_ops op in
        let rmfit = rm ~sub:(`reg reg) in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons $anti:rmfit$ Empty) >>

    | op = OP; reg = reg8; ","; n = num8 ->
        let opbyte,sub = rm8_imm8_ops op in
        let modrmbyte = modrm `direct (`subcode sub) (`reg reg) in
        let opbstr = string_of_int opbyte
        and mrmstr = string_of_int modrmbyte in
          <:expr< lazy_cons (Bytes [$int:opbstr$; $int:mrmstr$])
                    (lazy_cons $n$ Empty) >>

    | op = OP; "byte"; rm = rm32; ","; rcl = reg8_cl_only ->
        begin let opbyte,sub = rm8_cl_ops op in
        let rmfit = rm ~sub:(`subcode sub)
        and opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons $anti:rmfit$ Empty) >>
        end

    | op = OP; "byte"; rm = rm32; ","; n = num8 ->
        let opbyte,sub = rm8_imm8_ops op in
        let rmfit = rm ~sub:(`subcode sub) in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons $anti:rmfit$
                      (lazy_cons $n$ Empty)) >>

    | op = OP; "byte"; rm = rm32; ","; reg = reg8 ->
        let prefix,opbyte = rm8_reg8_ops op in
        let rmfit = rm ~sub:(`reg reg) in
        let opbstr = string_of_int opbyte
        and prestr = prefstr prefix in
          <:expr< lazy_cons $anti:prestr$
                    (lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$ Empty)) >>

    | op = OP; reg = reg8; ","; "byte"; rm = rm32 ->
        let opbyte = reg8_rm8_ops op in
        let rmfit = rm ~sub:(`reg reg) in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons $anti:rmfit$ Empty) >>

    (* Pretend we have this instruction, as well. *)
    | op = OP; reg = reg8_al_only; ","; reg2 = reg8 ->
        let opbyte = reg8_rm8_ops op in
        let rm = string_of_int (modrm `direct (`reg reg) (`reg reg2)) in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Bytes [$int:opbstr$; $int:rm$]) Empty >>

    (* Pretend we have this instruction, sort of. *)
    | op = OP; reg = reg8; ","; reg2 = reg8 ->
        let opbyte = reg8_rm8_ops op in
        let rm = string_of_int (modrm `direct (`reg reg) (`reg reg2)) in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Bytes [$int:opbstr$; $int:rm$]) Empty >>

    (* Use:
        accumulator mode for immediates which don't fit in 8 bits
        rm mode for immediates which do (even for eax)
        rm mode for non-eax
    *)
    | op = OP; regnum = reg32; ","; n = num32 ->
        begin let fits8,nval = n in
        try
          if regnum <> 0 || (match fits8 with `fit _ -> true | _ -> false)
            then raise BadFormat;
          let opbyte = string_of_int (eax_imm32_ops op)
          in
            <:expr< lazy_cons (Byte $int:opbyte$)
                      (lazy_cons ($nval$) Empty) >>
        with BadFormat ->
        try
          let n8 =
            match fits8 with
              `nofit -> raise BadFormat
            | `fit n -> n
          in
          let prefix,opbyte,sub = rm32_imm8_ops op in
          let modrmbyte = modrm `direct (`subcode sub) (`reg regnum) in
          let opbstr = string_of_int opbyte
          and mrmstr = string_of_int modrmbyte
          and prestrl = prefstr prefix
          in
            <:expr< lazy_cons $anti:prestrl$
                      (lazy_cons (Bytes [$int:opbstr$; $int:mrmstr$])
                        (lazy_cons $n8$ Empty)) >>
        with BadFormat ->
          let opbyte,sub = rm32_imm32_ops op in
          let modrmbyte = modrm `direct (`subcode sub) (`reg regnum) in
          let opbstr = string_of_int opbyte
          and mrmstr = string_of_int modrmbyte
          in
            <:expr< lazy_cons (Bytes [$int:opbstr$; $int:mrmstr$])
                      (lazy_cons $nval$ Empty) >>
        end

    | op = OP; regnum = reg32; ","; rcl = reg8_cl_only ->
        begin let opbyte,sub = rm32_cl_ops op in
        let rm = string_of_int (modrm `direct (`subcode sub) (`reg regnum))
        and opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Bytes [$int:opbstr$; $int:rm$]) Empty >>
        end

    | op = OP; regnum = reg32; ","; "byte"; rm = rm32 ->
        let prefix,opbyte = extend_byte_ops op in
        let rmfit = rm ~sub:(`reg regnum)
        and opbstr = string_of_int opbyte
        and prestr = prefstr prefix in
          <:expr< lazy_cons $anti:prestr$
                    (lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$ Empty)) >>

    | op = OP; regnum = reg32; ","; "word"; rm = rm32 ->
        let prefix,opbyte = extend_word_ops op in
        let rmfit = rm ~sub:(`reg regnum)
        and opbstr = string_of_int opbyte
        and prestr = prefstr prefix in
          <:expr< lazy_cons $anti:prestr$
                    (lazy_cons (Byte $int:opbstr$)
                       (lazy_cons $anti:rmfit$ Empty)) >>

    | op = OP; "dword"; rm = rm32; ","; rcl = reg8_cl_only ->
        begin let opbyte,sub = rm32_cl_ops op in
        let rmfit = rm ~sub:(`subcode sub)
        and opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons $anti:rmfit$ Empty) >>
        end

    | op = OP; "dword"; rm = rm32; ","; n = num32 ->
        let fits8,nval = n in
        let longer () = 
          let opbyte,sub = rm32_imm32_ops op in
          let opbstr = string_of_int opbyte in
          let rmfit = rm ~sub:(`subcode sub)
          in
            <:expr< lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$
                         (lazy_cons $nval$ Empty)) >>
        in begin match fits8 with
          `nofit -> longer ()
        | `fit n8 ->
          try
            let prefix,opbyte,sub = rm32_imm8_ops op in
            let prestr = prefstr prefix
            and opbstr = string_of_int opbyte in
            let rmfit = rm ~sub:(`subcode sub)
            in
              <:expr< lazy_cons $anti:prestr$
                        (lazy_cons (Byte $int:opbstr$)
                          (lazy_cons $anti:rmfit$
                            (lazy_cons $n8$ Empty))) >>
          with BadFormat ->
            longer ()
        end
    
    | op = OP; regnum = reg32; ","; rm = rm32 ->
        begin let prefix,opbyte = reg32_rm32_ops op in
        let opbstr = string_of_int opbyte
        and rmfit = rm ~sub:(`reg regnum)
        and prestr = prefstr prefix in
          <:expr< lazy_cons $anti:prestr$
                    (lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$ Empty)) >>
        end

    | op = OP; "dword"; rm = rm32; ","; regnum = reg32 ->
        begin let prefix,opbyte = rm32_reg32_ops op in
        let prestr = prefstr prefix
        and rmfit = rm ~sub:(`reg regnum)
        and opbstr = string_of_int opbyte in
          <:expr< lazy_cons $anti:prestr$
                    (lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$ Empty)) >>
        end

    | op = OP; "dword"; rm = rm32; ","; regnum = reg32; ","; n = num8 ->
        let prefix,opbyte = rm32_reg32_imm8_ops op in
        let prestr = prefstr prefix
        and opbstr = string_of_int opbyte
        and rmfit = rm ~sub:(`reg regnum) in
          <:expr< lazy_cons ($anti:prestr$)
                    (lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$
                        (lazy_cons $n$ Empty))) >>

  (*  | op = OP; reg1 = reg32; ","; reg2 = reg32; ","; "#"; n = NUM ->
        let prefix,opbyte = rm32_reg32_imm8_ops op in
        let prestr = prefstr prefix
        and opbstr = string_of_int opbyte
        and rmbyte = string_of_int (modrm `direct (`reg reg1) (`reg reg2)) in
          <:expr< $anti:prestr$ @ [$int:opbstr$; $int:rmbyte$; $int:n$] >> *)

    | op = OP; reg = reg32; ","; "dword"; rm = rm32; ","; n = num32 ->
        let fits8,nval = n in
        begin match fits8 with
          `fit n8 ->
            let opbyte = string_of_int (reg32_rm32_imm8_ops op)
            and rmfit = rm ~sub:(`reg reg) in
              <:expr< lazy_cons (Byte $int:opbyte$)
                        (lazy_cons $anti:rmfit$
                          (lazy_cons $n8$ Empty)) >>
        | `nofit ->
            let opbyte = string_of_int (reg32_rm32_imm32_ops op)
            and rmfit = rm ~sub:(`reg reg)
            in
              <:expr< lazy_cons (Byte $int:opbyte$)
                        (lazy_cons $anti:rmfit$
                          (lazy_cons $nval$ Empty)) >>
        end

    | op = OP; regnum = reg32 ->
        begin let prefix,opbyte = reg32_ops regnum op in
        let prestr = prefstr prefix in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons ($anti:prestr$)
                     (lazy_cons (Byte $int:opbstr$) Empty) >>
        end
        
    | op = OP; "dword"; rm = rm32 ->
        begin let opbyte,sub = rm32_ops op in
        let rmfit = rm ~sub:(`subcode sub)
        and opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons $anti:rmfit$ Empty) >>
        end

    | op = OP; "byte"; rm = rm32 ->
        begin let prefix,opbyte,sub = rm8_ops op in
        let rmfit = rm ~sub:(`subcode sub)
        and opbstr = string_of_int opbyte
        and prestr = prefstr prefix
        in
          <:expr< lazy_cons ($anti:prestr$) 
                    (lazy_cons (Byte $int:opbstr$)
                      (lazy_cons $anti:rmfit$ Empty)) >>
        end

    | op = OP; "byte"; n = num8 ->
        let opbstr = string_of_int (imm8_ops op) in
          <:expr< lazy_cons (Byte $int:opbstr$)
                    (lazy_cons ($n$) Empty) >>
    
    | op = OP; "dword"; n = num32 ->
        let opbstr = string_of_int (imm32_ops op)
        and _,nval = n
        in
          <:expr< lazy_cons (Byte $int:opbstr$) (lazy_cons $nval$ Empty) >>

    | op = OP ->
        begin let opbyte = narg_ops op in
        let opbstr = string_of_int opbyte in
          <:expr< lazy_cons (Byte $int:opbstr$) Empty >>
        end
    
    | op = OP; "near"; n = num8 ->
        let opbyte = string_of_int (jump_near_ops op) in
          <:expr< lazy_cons (Byte $int:opbyte$)
                    (lazy_cons $n$ Empty) >>

    | op = OP; "far"; n = num32 ->
        let prefix,opbyte = jump_far_ops op in
        let opbstr = string_of_int opbyte
        and _,nval = n
        and prestr = prefstr prefix in
          <:expr< lazy_cons $anti:prestr$
                    (lazy_cons (Byte $int:opbstr$)
                       (lazy_cons $nval$ Empty)) >>

    ]];

  num8:
    [[
      "#"; n = NUM -> <:expr< Byte $int:n$ >>
    | "$"; i = REL; "$" -> <:expr< Rel8 $lid:i$ >>
    | "$"; i = ABS; "$" -> <:expr< Abs8 $lid:i$ >>
    ]];

  (* Boolean is "fits in 8 sign-extended bits" *)
  num32:
    [[
      "#"; n = NUM ->
        let q = makelist (List.map
          (fun n -> let m = Int32.to_string n in <:expr< $int:m$ >>)
          (split_int32 (Int32.of_string n)))
        and v = Int32.of_string n
        in
          let mumble = if v >= -128l && v < 128l then
            (`fit <:expr< Byte $int:n$ >>)
          else
            `nofit
          in (mumble, <:expr< Bytes $q$ >>)
    | "$"; i = REL; "$" -> (`nofit, <:expr< Rel32 $lid:i$ >>)
    | "$"; i = ABS; "$" -> (`nofit, <:expr< Abs32 $lid:i$ >>)
    ]];

END

let add_quot_x86 x86 name =
  let pat s = failwith "patt type not implemented" in
  let exp s = Grammar.Entry.parse x86 (Stream.of_string s) in
    Quotation.add name (Quotation.ExAst (exp, pat))

let _ = add_quot_x86 x86 "x86"
