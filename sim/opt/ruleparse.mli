val rule_lexer : Token.lexer
val gram : Grammar.g
val insn_match : MLast.expr Grammar.Entry.e
(* val loc : int * int *)
(* val _loc : Lexing.position * Lexing.position *)
val add_quot_rule : MLast.expr Grammar.Entry.e -> string -> unit
