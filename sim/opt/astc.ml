(* A (hopefully semantic-preserving) C pretty-printer for AST *)

open Ast

exception BadPlaceForTreg

let rec print_triop op a b c =
  match op with
    Branch -> Printf.sprintf "if ((uint5)" ^ (print_node a) ^ ")\n" ^
      "{\n" ^ print_cflow b ^ "}\nelse\n{" ^ print_cflow c ^ "}\n"

and print_cflow x =
  match x with
    Constant c -> "goto *idx[" ^ (Int32.to_string (Int32.div c 16l)) ^ "];"
  | Register r -> "goto *idx[" ^ print_reg r ^ "];"
  | _ -> failwith "Unsupported flow construct"

and print_binop op a b =
  match op with
    Add -> print_node a ^ "+" ^ print_node b
  | Sub -> print_node a ^ "-" ^ print_node b
  | Mul -> print_node a ^ "*" ^ print_node b
  | Div -> "(sint5)" ^ print_node a ^ "/" ^ print_node b
  | Lsl -> print_node a ^ "<<" ^ print_node b
  | Lsr -> print_node a ^ ">>" ^ print_node b
  | Asr -> "(sint5)" ^ print_node a ^ ">>" ^ print_node b
  | Ror -> "ROR(" ^ print_node a ^ "," ^ print_node b ^ ")"
  | And -> print_node a ^ "&" ^ print_node b
  | Ior -> print_node a ^ "|" ^ print_node b
  | Eor -> print_node a ^ "^" ^ print_node b
  | Cmp n -> "(" ^ print_condition n a b ^ ") ? 0xffffffff : 0x0"
  | Udiv -> print_node a ^ "/" ^ print_node b
  | Mod -> "(sint5)" ^ print_node a ^ "%" ^ print_node b
  | Umod -> print_node a ^ "%" ^ print_node b
  | Call -> "r[62] = r[63]; " ^ print_cflow a
  | _ -> failwith "Unimplemented binop"

and print_reg r =
  let ((num, loc), Id.IntType, expi) = r in
  "r[" ^ string_of_int num ^ "]"

and print_condition c a b =
  match c with
    Iformat.Eq -> print_node a ^ "==" ^ print_node b
  | Iformat.Ne -> print_node a ^ "!=" ^ print_node b
  | Iformat.Ge -> "(sint5)" ^ print_node a ^ ">=" ^ "(sint5)" ^ print_node b
  | Iformat.Gt -> "(sint5)" ^ print_node a ^ ">" ^ "(sint5)" ^ print_node b
  | Iformat.Le -> "(sint5)" ^ print_node a ^ "<=" ^ "(sint5)" ^ print_node b
  | Iformat.Lt -> "(sint5)" ^ print_node a ^ "<" ^ "(sint5)" ^ print_node b
  | Iformat.Geu -> print_node a ^ ">=" ^ print_node b
  | Iformat.Gtu -> print_node a ^ ">" ^ print_node b
  | Iformat.Leu -> print_node a ^ "<=" ^ print_node b
  | Iformat.Ltu -> print_node a ^ "<" ^ print_node b
  | _ -> failwith "Unimplemented condition"

and print_unop op a =
  match op with
    Not -> "~" ^ print_node a
  | Ind(dom,w) -> print_ind w a
  | Jump -> print_cflow a

and print_nop op =
  match op with
    Return -> print_cflow (Ast.Register((62, Id.Unset), Id.IntType, Use))
  | _ -> failwith "Bad nop"

and print_ind i a =
  match i with
    Byte -> "bytes[" ^ print_node a ^ "]"
  | Halfword -> "halfs[(" ^ print_node a ^ ")/2]"
  | Word -> "words[(" ^ print_node a ^ ")/4]"

and print_move a b =
  print_node a ^ "=" ^ print_node b ^ ";"

and print_constant c =
  Int32.to_string c

and print_node node =
  match node with
    Triop(op3,a,b,c) -> print_triop op3 a b c
  | Binop(op2,a,b) -> print_binop op2 a b
  | Unop(op1,a) -> print_unop op1 a
  | Nop(op0) -> print_nop op0
  | Move(a,b) -> print_move a b
  | Constant(c) -> print_constant c
 (* | Floatconst(f) -> print_floatconst f *)
  | Register(r) -> print_reg r
 (* | Floatreg(r) -> print_floatreg r
  | Treg(t) -> failwith "Bad place for treg, run mergeldt pass first"

let print_alist l =
  List.iter (fun x -> Printf.printf "%s\n" (print_node x)) l
