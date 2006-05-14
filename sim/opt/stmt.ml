(* Basic statement classification for optimisers *)

open Ast

type stmtclass = Assignment
               | PhiFunc
               | Load
               | Store
               | ControlFlow
               | NotStatement

let classify = function
    Move(_, Phi _) -> PhiFunc
  | Move(Unop(Ind _, _), _) -> Store
  | Move(_, Unop(Ind _, _)) -> Load
  | Move(_, _) -> Assignment
  | Zop(Return)
  | Unop(Jump, _)
  | Binop(Call, _, _)
  | Binop(Trap _, _, _)
  | Triop(Branch, _, _, _) -> ControlFlow
  | _ -> NotStatement

class stmt ?next:next_ast ?prev:prev_ast (initast : Ast.ast) =
  object(self)
    val mutable next = next_ast
    val mutable prev = prev_ast
    val mutable ast = initast
    val mutable classification = classify initast
        
    method ast = ast
    method set_ast x =
      ast <- x;
      classification <- classify x
    
    method classify = classification
  end
