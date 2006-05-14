(* A lexer for the assembly code expander *)

{
  let trim = function
      s -> String.sub s 1 ((String.length s) - 2)

  let unquote = function
    s -> 
	   let trimmed = trim s in
	   let len = String.length trimmed in
	   let qtype_len = String.index trimmed ':' in
	   let quote_type = String.sub trimmed 0 qtype_len in
	   let quote_val = String.sub trimmed (qtype_len + 1) (len - (qtype_len + 1))
     in
	     let upper_type = "ANTI_" ^ (String.uppercase quote_type) in
	       (upper_type, quote_val)
}

let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']+
let chars = [',' '.' ';' '!' '#' '@' '[' ']' '{' '}' '<' '>' '~' '-'
             'e' 'f' 'l' 'h' 'b' 'h' 'w' 's' 'd' 'v' '*']
let quote = "imm" | "reg" | "dreg"

rule asm_token = parse
    'r'           { ("", Lexing.lexeme lexbuf) }
  | num+          { ("INT", Lexing.lexeme lexbuf) }
  | chars         { ("", Lexing.lexeme lexbuf) }
  | eof           { ("EOI", "") }
  | "mov"         { ("OP2", Lexing.lexeme lexbuf) }
  | "not"         { ("OP2", Lexing.lexeme lexbuf) }
  | "lsl"         { ("OP3", Lexing.lexeme lexbuf) }
  | "lsr"         { ("OP3", Lexing.lexeme lexbuf) }
  | "asr"         { ("OP3", Lexing.lexeme lexbuf) }
  | "ror"         { ("OP3", Lexing.lexeme lexbuf) }
  | "and"         { ("OP3", Lexing.lexeme lexbuf) }
  | "ior"         { ("OP3", Lexing.lexeme lexbuf) }
  | "eor"         { ("OP3", Lexing.lexeme lexbuf) }
  | "bic"         { ("OP3", Lexing.lexeme lexbuf) }
  | "add"         { ("OP3", Lexing.lexeme lexbuf) }
  | "sub"         { ("OP3", Lexing.lexeme lexbuf) }
  | "rsb"         { ("OP3", Lexing.lexeme lexbuf) }
  | "mul"         { ("OP3", Lexing.lexeme lexbuf) }
  | "div"         { ("OP3", Lexing.lexeme lexbuf) }
  | "udiv"        { ("OP3", Lexing.lexeme lexbuf) }
  | "mod"         { ("OP3", Lexing.lexeme lexbuf) }
  | "umod"        { ("OP3", Lexing.lexeme lexbuf) }
  | "movf"        { ("FP2", Lexing.lexeme lexbuf) }
  | "negf"        { ("FP2", Lexing.lexeme lexbuf) }
  | "sqrf"        { ("FP2", Lexing.lexeme lexbuf) }
  | "absf"        { ("FP2", Lexing.lexeme lexbuf) }
  | "addf"        { ("FP3", Lexing.lexeme lexbuf) }
  | "subf"        { ("FP3", Lexing.lexeme lexbuf) }
  | "mulf"        { ("FP3", Lexing.lexeme lexbuf) }
  | "divf"        { ("FP3", Lexing.lexeme lexbuf) }
  | "mvc"         { ("MVC", "") }
  | "ldr"         { ("LDR", "") }
  | "str"         { ("STR", "") }
  | "cmp"         { ("CMP", "") }
  | "ucmp"        { ("UCMP", "") }
  | "bfx"         { ("BFX", "") }
  | "bfi"         { ("BFI", "") }
  | "stm"         { ("STM", "") }
  | "ldm"         { ("LDM", "") }
  | "ldf"         { ("LDF", "") }
  | "stf"         { ("STF", "") }
  | "ldmf"        { ("LDMF", "") }
  | "stmf"        { ("STMF", "") }
  | "call"        { ("CALL", "") }
  | "cbr"         { ("CBR", "") }
  | "jump"        { ("JUMP", "") }
  | "ret"         { ("RET", "") }
  | "swi"         { ("SWI", "") }
  | "trap"        { ("TRAP", "") }
  | "utrap"       { ("UTRAP", "") }
  | "nop"         { ("NOP", "") }
  | "ldx"         { ("LDT", Lexing.lexeme lexbuf) }
  | "ldy"         { ("LDT", Lexing.lexeme lexbuf) }
  | "ldz"         { ("LDT", Lexing.lexeme lexbuf) }
  | "eq"          { ("COMP", Lexing.lexeme lexbuf) }
  | "ne"          { ("COMP", Lexing.lexeme lexbuf) }
  | "lt"          { ("COMP", Lexing.lexeme lexbuf) }
  | "le"          { ("COMP", Lexing.lexeme lexbuf) }
  | "gt"          { ("COMP", Lexing.lexeme lexbuf) }
  | "ge"          { ("COMP", Lexing.lexeme lexbuf) }
  | "n"           { ("TRPC", Lexing.lexeme lexbuf) }
  | "z"           { ("TRPC", Lexing.lexeme lexbuf) }
  | '$' quote ':' [^'$']+ '$'  { unquote (Lexing.lexeme lexbuf) }
  | "`r"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDREG", String.sub str 2 ((String.length str)-2)) }
  | "`f"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDFREG", String.sub str 2 ((String.length str)-2)) }
  | "`#"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDIMM", String.sub str 2 ((String.length str)-2)) }
  | "`@"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDDOM", String.sub str 2 ((String.length str)-2)) }
  | "`n"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDNUM", String.sub str 2 ((String.length str)-2)) }
  | "`<"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDHIBITS", String.sub str 2 ((String.length str)-2)) }
  | "`>"alpha     { let str = Lexing.lexeme lexbuf in
                    ("QUOTEDLOBITS", String.sub str 2 ((String.length str)-2)) }
  | _             { asm_token lexbuf }
