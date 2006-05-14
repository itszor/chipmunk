(* Lexer for AST rule expander *)

{

}

let num = ['0'-'9']+
let hexnum = ['0'-'9''A'-'F''a'-'f']+
let alpha = ['a'-'z''A'-'Z']
let id = alpha+ (num | alpha)*
let operators ="lsl"|"lsr"|"asr"|"ror"|"&"|"|"|"^"|"+"|"-"|"*"
              |"/"|"%"|"/u"|"%u"|"+s"|"+d"|"-s"|"-d"|"*s"|"*d"|"/s"|"/d"
let comparisons =  "=="|"!="|"<="|"<"|">="|">"|"<=u"|"<u"|">=u"|">u"
let transfers = "cbr"|"call"|"jump"|"ret"|"trapz"|"trapn"|"nop"
let miscops = "phi"|"phi_passthrough"
let brackets = "("|")"|"["|"]"
let space = " "|"\t"|"\n"

rule rule_token = parse
    'r'alpha      { ("REG", Lexing.lexeme lexbuf) }
  | 'f'alpha      { ("FREG", Lexing.lexeme lexbuf) }
  | "<-"          { ("BECOMES", Lexing.lexeme lexbuf) }
  | "~"           { ("", Lexing.lexeme lexbuf) }
  | operators     { ("", Lexing.lexeme lexbuf) }
  | comparisons   { ("", Lexing.lexeme lexbuf) }
  | transfers     { ("", Lexing.lexeme lexbuf) }
  | miscops       { ("", Lexing.lexeme lexbuf) }
  | brackets      { ("", Lexing.lexeme lexbuf) }
  | "@"|","       { ("", Lexing.lexeme lexbuf) }
  | "0x"hexnum    { ("NUM", Lexing.lexeme lexbuf) }
  | "s#"          { ("SHIFTIMM", Lexing.lexeme lexbuf) }
  | "d#"          { ("DATAIMM", Lexing.lexeme lexbuf) }
  | "f#"          { ("FLOATIMM", Lexing.lexeme lexbuf) }
  | "nd#"         { ("INVDATAIMM", Lexing.lexeme lexbuf) }
  | "md#"         { ("MAYBENEGDATAIMM", Lexing.lexeme lexbuf) }
  | "eh#"         { ("EHIMM", Lexing.lexeme lexbuf) }
  | "el#"         { ("ELIMM", Lexing.lexeme lexbuf) }
  | "fh#"         { ("FHIMM", Lexing.lexeme lexbuf) }
  | "fl#"         { ("FLIMM", Lexing.lexeme lexbuf) }
  | "dm#"         { ("DWORDMEMIMM", Lexing.lexeme lexbuf) }
  | "wm#"         { ("WORDMEMIMM", Lexing.lexeme lexbuf) }
  | "hm#"         { ("HALFMEMIMM", Lexing.lexeme lexbuf) }
  | "bm#"         { ("BYTEMEMIMM", Lexing.lexeme lexbuf) }
  | "wordvec"     { ("WORDVEC", Lexing.lexeme lexbuf) }
  | "sword"       { ("SWORD", Lexing.lexeme lexbuf) }
  | "dword"       { ("DWORD", Lexing.lexeme lexbuf) }
  | "dwordvec"    { ("DWORDVEC", Lexing.lexeme lexbuf) }
  | "word"        { ("WORD", Lexing.lexeme lexbuf) }
  | "half"        { ("HALF", Lexing.lexeme lexbuf) }
  | "byte"        { ("BYTE", Lexing.lexeme lexbuf) }
  | "bitf"        { ("BITF", Lexing.lexeme lexbuf) }
  | "sbitf"       { ("SBITF", Lexing.lexeme lexbuf) }
  | alpha         { ("ALPHA", Lexing.lexeme lexbuf) }
  | "#"           { ("IMM", Lexing.lexeme lexbuf) }
  | "..."         { ("DOTS", Lexing.lexeme lexbuf) }
  | "ign"         { ("IGNORE", Lexing.lexeme lexbuf) }
  | "tmp"         { ("TEMP", Lexing.lexeme lexbuf) }
  | ":="          { ("DECLARE", Lexing.lexeme lexbuf) }
  | "=>"          { ("DEFINE", Lexing.lexeme lexbuf) }
  | "{" (_#"}")* "}"    { ("ASSEMBLER", Lexing.lexeme lexbuf) }
  | "$" (_#"$")* "$"    { ("CAML", Lexing.lexeme lexbuf) }
  | ":"           { ("COLON", Lexing.lexeme lexbuf) }
  | ";"           { ("EOL", Lexing.lexeme lexbuf) }
  | "sgl"         { ("SGL", Lexing.lexeme lexbuf) }
  | "dbl"         { ("DBL", Lexing.lexeme lexbuf) }
  | num           { ("DEC", Lexing.lexeme lexbuf) }
  | id            { ("ID", Lexing.lexeme lexbuf) }
  | eof           { ("EOI", "") }
  | space         { rule_token lexbuf }
(*  | _             { rule_token lexbuf } *)
