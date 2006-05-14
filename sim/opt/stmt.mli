type stmtclass = Assignment
               | PhiFunc
               | Load
               | Store
               | ControlFlow
               | NotStatement

val classify : Ast.ast -> stmtclass
