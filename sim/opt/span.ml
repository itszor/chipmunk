(* Functions for finding & maintaining register spans.
  (For simplicity, number registers:
      0-63  : int regs
     64-127 : float regs)
*)

open Sets

type span = {
  born: int option;
  dies: int option;
  usedat: int list;
  broken: bool;
}

let findspan ast init =
  let update id life accctr =
    let acc,ctr = accctr in
    if SpanMap.mem id acc then
      let prev = SpanMap.find id acc in
      match life with
        Ast.LastUse ->
          (SpanMap.add id {prev with dies=Some ctr} acc, ctr)
      | Ast.Use ->
        (SpanMap.add id {prev with usedat=ctr::prev.usedat} acc, ctr)
      | Ast.Assign
      | Ast.FixedAssign ->
        (SpanMap.add id {prev with born=Some ctr} acc, ctr)
    else
      match life with
        Ast.LastUse ->
          (SpanMap.add id {born=None; dies=Some ctr;
                           usedat=[]; broken=false} acc, ctr)
      | Ast.Use ->
          (SpanMap.add id {born=None; dies=None;
                           usedat=[ctr]; broken=false} acc, ctr)
      | Ast.Assign
      | Ast.FixedAssign ->
          (SpanMap.add id {born=Some ctr; dies=None;
                           usedat=[]; broken=false} acc, ctr)
  in
    Ast.fold_rtol_postorder
      (fun node accctr ->
        let acc,ctr = accctr in
        match node with
          Ast.Register(id, usage) ->
            update (Id.PhysRegi id) usage (acc, ctr+1)
        | Ast.Floatreg(id, usage) ->
            update (Id.PhysRegf id) usage (acc, ctr+1)
        | Ast.Intpseudo(num, usage) ->
            update (Id.Pseudoi num) usage (acc, ctr+1)
        | Ast.Floatpseudo(num, usage) ->
            update (Id.Pseudof num) usage (acc, ctr+1)
        | _ -> (acc, ctr+1))
      ast
      init

let findspans asts =
  let spans,_ = 
    List.fold_right (fun ast acc -> findspan ast acc) asts (SpanMap.empty, 0)
  in
    spans

(* Worryingly O(n^2)-looking *)
let breakspans asts spans =
  let breakspan ast spans =
    let break at id span =
      match span.born,span.dies with
        Some b, Some d ->
          if at>=b && at<=d then
            {span with broken=true}
          else
            span
      | _ -> span
    in Ast.fold_rtol_postorder
      (fun node accctr ->
        let acc,ctr = accctr in
        match node with
          Ast.Binop(Ast.Trap _, _, _) ->
            (SpanMap.mapi (break ctr) acc, ctr+1)
        | _ -> (acc, ctr+1))
      ast
      spans
  in
    let spans, _ =
      List.fold_right (fun ast acc -> breakspan ast acc) asts (spans, 0)
    in
      spans

let printspans spans =
  let print_option x =
    match x with
      None -> "never"
    | Some q -> string_of_int q
  and print_reg_or_pseudo = function
      Id.PhysRegi(num,loc) ->
        Printf.sprintf "r%d[%s]" num (Disast.writeloc loc)
    | Id.PhysRegf(num,loc) ->
        Printf.sprintf "f%d[%s]" num (Disast.writeloc loc)
    | Id.Pseudoi(num) ->
        Printf.sprintf "ip%d" num
    | Id.Pseudof(num) ->
        Printf.sprintf "fp%d" num
  in
  SpanMap.iter (fun id span ->
    Printf.printf "%s: born %s, dies %s, %s\n"
      (print_reg_or_pseudo id)
      (print_option span.born)
      (print_option span.dies)
      (if span.broken then "broken" else "unbroken"))
    spans

let substitute_pseudos asts spans =
  let rewrite id pseudo asts =
    List.map
      (fun ast -> Ast.map_postorder
        (fun x ->
          match x,id with
            Ast.Register((num',loc'),life),Id.PhysRegi(num,loc)
                when num'=num && loc'=loc ->
              Ast.Intpseudo(pseudo, life)
          | Ast.Floatreg((num',loc'),life),Id.PhysRegf(num,loc)
                when num'=num && loc'=loc ->
              Ast.Floatpseudo(pseudo, life)
          | x,_ -> x)
        ast)
      asts
  in
    let res, _ = SpanMap.fold
      (fun id span (asts,ps) ->
        match span.born, span.dies, span.broken with
          Some p, Some q, false -> rewrite id ps asts, ps+1
        | _ -> asts,ps+1)
      spans
      (asts,0)
    in
      List.rev res
