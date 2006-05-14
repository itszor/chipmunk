(* This file is BROKEN by typed registers! *)

open Ast

exception FailReg of string * Id.id * Sets.IdSet.t
exception FailPseud of string * int

let write_id_set ids =
  Sets.IdSet.iter (fun (num,loc) ->
    Printf.printf "r%d[%s]\n" num (Disast.writeloc loc)) ids

(* Checks sanity of register-expiry info.
 * only does int registers!
 *)
let verify_register_expiry asts =
  try
    ignore (List.fold_right (fun ast start ->
      fold_rtol_postorder
        (fun node acc ->
          let ids,pseuds = acc in
          match node with
            Register(id,typ,life) ->
              begin match life with
                LastUse ->
                  if Sets.IdSet.mem id ids then
                    raise (FailReg("Attempt to expire dead register", id, ids))
                  else
                    Sets.IdSet.add id ids,pseuds
              | Use ->
                  if Sets.IdSet.mem id ids then
                    raise (FailReg("Attempt to use dead register", id, ids))
                  else
                    acc
              | Assign | FixedAssign ->
                  if Sets.IdSet.mem id ids then
                    Sets.IdSet.remove id ids,pseuds
                  else
                    acc
              end
          | Pseudo(num,typ,life) ->
              begin match life with
                LastUse ->
                  if Sets.IntSet.mem num pseuds then
                    raise (FailPseud("Attempt to expire dead pseudo", num))
                  else
                    ids,Sets.IntSet.add num pseuds
              | Use ->
                  if Sets.IntSet.mem num pseuds then
                    raise (FailPseud("Attempt to use dead pseudo", num))
                  else
                    acc
              | Assign | FixedAssign ->
                  if Sets.IntSet.mem num pseuds then
                    ids,Sets.IntSet.remove num pseuds
                  else
                    acc
              end
          | _ -> acc)
        ast
        start)
      asts
      (Sets.IdSet.empty,Sets.IntSet.empty))
  with
    FailReg(msg,(num,loc),ids) ->
      Printf.printf "%s r%d[%s]\n" msg num (Disast.writeloc loc);
      write_id_set ids;
      exit 0
  | FailPseud(msg,num) ->  failwith
      (Printf.sprintf "%s ip%d" msg num)
