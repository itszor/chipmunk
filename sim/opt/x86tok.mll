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
let punct = "," | "#" | "b" | "l" | "-"

rule x86_token = parse
    reg8         { ("REG8", Lexing.lexeme lexbuf) }
  | reg32        { ("REG32", Lexing.lexeme lexbuf) }
  | ops          { ("OP", Lexing.lexeme lexbuf) }
  | punct        { ("", Lexing.lexeme lexbuf) }
  | num          { ("NUM", Lexing.lexeme lexbuf) }
  | eof          { ("EOI", "") }
  | _            { x86_token lexbuf }
