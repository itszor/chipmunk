type classification = LNone
                    | LInt of Ast.reg
                    | LFloat of Ast.reg
                    | LDouble of Ast.reg
                    | LIntPseudo of int
                    | LFloatPseudo of int
                    | LPartInt of Ast.reg
                    | LVector of Ast.ast list
                    | LMemory

val classify : Ast.ast -> classification
val rewritereg : Ast.ast -> classification -> Ast.ast -> Ast.ast
