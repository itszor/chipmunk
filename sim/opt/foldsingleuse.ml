(* If a register becomes dead, it was defined in this block, and it has
   only one usage in this block, replace with definition. This allows us
   more flexibility for register allocation later.
*)

open Ast
open Block
open Usedef

let collapse astlist counts =
  ()

(*
let collapse astlist counts =
  let ifolded = ref IdSet.empty
  and ffolded = ref IdSet.empty
  in let undefine tbl id =
    Hashtbl.remove tbl id;
    Hashtbl.add tbl id (Nop Noop)
  in let rec scan = function
      Triop(op,a,b,c) -> Triop(op, scan a, scan b, scan c)
    | Binop(op,a,b) -> Binop(op, scan a, scan b)
    | Unop(op,a) -> Unop(op, scan a)
    | Nop(op) as m -> m
    | Move(a,b) -> Move(scan a, scan b)
    | Pair(a,b) -> Pair(scan a, scan b)
    | Register(id,expi) as m ->
        begin try match expi with
          Iformat.Dead ->
            let def = Hashtbl.find counts.intdef id
            and uses = !(Hashtbl.find counts.intuse id) in
            if uses = 1 then begin
              def.ireginfo.note <- RegnoteSet.add (UsageFolded id) 
                                   def.ireginfo.note;
              ifolded := IdSet.add id !ifolded;
              def
            end else m
        | _ -> m
        with Not_found -> m
        end
    | Floatreg(id,expi) as m ->
        begin try match expi with
          Iformat.Dead ->
            let def = Hashtbl.find counts.fltdef id
            and uses = !(Hashtbl.find counts.fltuse id) in
            if uses = 1 then begin
              def.freginfo.note <- RegnoteSet.add (UsageFolded id) 
                                   def.freginfo.note;
              ffolded := IdSet.add id !ffolded;
              def
            end else m
        | _ -> m
        with Not_found -> m
        end
    | m -> m
  in
    let newasts = List.map scan astlist in
    (newasts, !ifolded, !ffolded)

let definedids note =
  let scan el ids =
    match el with
      Defined id -> IdSet.add id ids
    | _ -> ids
  in
    RegnoteSet.fold scan note IdSet.empty

let writeids x =
  IdSet.iter (fun (num,loc) -> Printf.printf "%s\n"
    (Disast.writereg num loc Iformat.Alive)) x

(* Intersection of definedids at a particular place and all ids which are
   expired.
   Means: Defined here AND has been folded if non-empty
   So, kill it.
*)
let undef memos ifolded ffolded =
  let rec scan = function
    [] -> []
  | thisnode :: rest ->
      if not (IdSet.is_empty
               (IdSet.inter (definedids thisnode.ireginfo.note)
                            ifolded))
      then
        scan rest
      else
        thisnode :: scan rest
  in
    scan memos
*)
