(* Lexer for x86 asm expander *)
{

}

let num = ['0'-'9']+
let reg8 = "al" | "cl" | "dl" | "bl" | "ah" | "ch" | "dh" | "bh"
let reg32 = "eax" | "ecx" | "edx" | "ebx" | "esp" | "ebp" | "esi" | "edi"
let ops = "add" | "or" | "adc" | "sbb" | "and" | "sub" | "xor" | "cmp"
        | "test" | "rol" | "ror" | "rcl" | "rcr" | "shl" | "shr" | "sar"
        | "mov" | "bt" | "bts" | "btr" | "btc" | "xadd" | "xchg" | "imul"
        | "bsf" | "bsr" | "inc" | "dec" | "not" | "neg" | "mul" | "div"
        | "idiv" | "pop" | "push" | "call" | "jmp" | "jecxz"
        | "enter" | "ret" | "retf"
        | "jo" | "jno" | "jb" | "jae" | "je" | "jne" | "jbe" | "ja"
        | "js" | "jns" | "jpe" | "jpo" | "jl" | "jge" | "jle" | "jg"
        | "pusha" | "popa" | "outsb" | "outs" | "nop" | "pushf"
        | "popf" | "sahf" | "lahf" | "movsb" | "movs" | "stosb" 
        | "stos" | "scasb" | "scas" | "leave" | "lock" | "cmc"
        | "clc" | "stc" | "cli" | "cld" | "std" | "sti"
        | "lea" | "loopnz" | "loopz" | "loop" | "movsx" | "movzx"
        | "seto" | "setno" | "setb" | "setae"
        | "sete" | "setne" | "setbe" | "seta"
        | "sets" | "setns" | "setpe" | "setpo"
        | "setl" | "setge" | "setle" | "setg"
        | "shld" | "shrd" | "xlat"

let al_imm8_ops = "add" | "or" | "adc" | "sbb" | "and"
                | "sub" | "xor" | "cmp" | "test"

let rm8_imm8_ops = "add" | "or" | "adc" | "sbb" | "and"
                 | "sub" | "xor" | "cmp" | "rol" | "ror"
                 | "rcl" | "rcr" | "shl" | "shr" | "sar"
                 | "mov" | "test"

let punct = "," | "#" | "-" | "+" | "*" | "[" | "]" | ";" | "$"
let whitespace = " " | "\t" | "\n"
let width = "byte" | "word" | "dword"
let dist = "near" | "far"
let ident = ['_''A'-'Z''a'-'z']['_''A'-'Z''a'-'z''0'-'9']*

rule x86_token = parse
    reg8         { ("", Lexing.lexeme lexbuf) }
  | reg32        { ("", Lexing.lexeme lexbuf) }
  | ops          { ("OP", Lexing.lexeme lexbuf) }
  | punct        { ("", Lexing.lexeme lexbuf) }
  | num          { ("NUM", Lexing.lexeme lexbuf) }
  | width        { ("", Lexing.lexeme lexbuf) }
  | dist         { ("", Lexing.lexeme lexbuf) }
  | "label"      { ("LABEL", Lexing.lexeme lexbuf) }
  | "rel:"ident  { let lex = Lexing.lexeme lexbuf in
                   ("REL", String.sub lex 4 ((String.length lex)-4)) }
  | "abs:"ident  { let lex = Lexing.lexeme lexbuf in
                   ("ABS", String.sub lex 4 ((String.length lex)-4)) }
  | ident        { ("ID", Lexing.lexeme lexbuf) }
  | whitespace+  { x86_token lexbuf }
  | "(*" _* "*)" { x86_token lexbuf }
  | eof          { ("EOI", "") }
