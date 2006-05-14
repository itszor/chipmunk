(* Parser for AST rule expander *)

#load "q_MLast.cmo";;
#load "pa_extend.cmo";;

open Ast
open Mtree

let rule_lexer =
  { Token.func = (Token.lexer_func_of_ocamllex Rulelex.rule_token);
    Token.using = (fun _ -> ());
    Token.removing = (fun _ -> ());
    Token.tparse = (fun _ -> None);
    Token.text = Token.lexer_text }

let gram = Grammar.create (rule_lexer);;

let insn_match = Grammar.Entry.create gram "insn_match";;

let _loc = Lexing.dummy_pos, Lexing.dummy_pos;;
(* let loc = 0,0;; *)

let ctr = ref 1;;

let name pref =
  ctr := !ctr + 1;
  pref ^ (string_of_int !ctr)

let ctr2 = ref 1;;

let mknum () =
  ctr2 := !ctr2 + 1;
  !ctr2

(* Immediate types would be better with a general type and a subtype for the
   particular immediate, so that a "narrower version of" test is easy.
   This is probably true for int reg/ptr reg too. *)

type matchingtree = Match_int_reg of string
                  | Match_ptr_reg of string
                  | Match_float_reg of string
                  | Match_int_dst_reg of string
                  | Match_ptr_dst_reg of string
                  | Match_float_dst_reg of string
                  | Match_imm of string * immflavour
                  | Match_const of int32
                  | Match_nop of op0
                  | Match_unop of op1 * matchingtree
                  | Match_binop of op2 * matchingtree * matchingtree
                  | Match_triop of op3 * matchingtree * matchingtree * 
                                   matchingtree
                  | Match_ignore of matchingtree
                  | Match_null
                  | Match_phi of string
                  | Match_move of matchingtree * matchingtree
                  | Match_gen_temp of string * matchingtree
                  | Match_factored of int list * string list * matchingtree

and defsuses = string list * string list

and immflavour = Shift_imm
               | Data_imm
               | Float_imm
               | Invdata_imm
               | Maybenegdata_imm
               | Eh_imm
               | El_imm
               | Fh_imm
               | Fl_imm
               | Wm_imm
               | Hm_imm
               | Bm_imm
               | Vanilla_imm

let string_of_flavour = function
    Shift_imm -> "shift"
  | Data_imm -> "data"
  | Float_imm -> "float"
  | Invdata_imm -> "invdata"
  | Maybenegdata_imm -> "maybenegdata"
  | Eh_imm -> "eh"
  | El_imm -> "el"
  | Fh_imm -> "fh"
  | Fl_imm -> "fl"
  | Wm_imm -> "wm"
  | Hm_imm -> "hm"
  | Bm_imm -> "bm"
  | Vanilla_imm -> "vanilla"

let rec print_matchingtree = function
    Match_int_reg n -> Printf.sprintf "(int_reg %s)" n
  | Match_ptr_reg n -> Printf.sprintf "(ptr_reg %s)" n
  | Match_float_reg n -> Printf.sprintf "(float_reg %s)" n
  | Match_int_dst_reg n -> Printf.sprintf "(int_dst_reg %s)" n
  | Match_ptr_dst_reg n -> Printf.sprintf "(ptr_dst_reg %s)" n
  | Match_float_dst_reg n -> Printf.sprintf "(float_dst_reg %s)" n
  | Match_imm(n,f) -> Printf.sprintf "(imm/%s %s)" (string_of_flavour f) n
  | Match_const n -> Printf.sprintf "(const %ld)" n
  | Match_nop _ -> "(nop)"
  | Match_unop(_,a) -> Printf.sprintf "(unop %s)" (print_matchingtree a)
  | Match_binop(_,a,b) -> Printf.sprintf "(binop %s %s)" (print_matchingtree a)
                            (print_matchingtree b)
  | Match_triop(_,a,b,c) -> Printf.sprintf "(triop %s %s %s)"
                              (print_matchingtree a)
                              (print_matchingtree b)
                              (print_matchingtree c)
  | Match_move(a,b) -> Printf.sprintf "(move %s %s)"
                         (print_matchingtree a)
                         (print_matchingtree b)
  | Match_gen_temp(a,b) -> Printf.sprintf "(gentemp %s %s)"
                             a
                             (print_matchingtree b)
  | Match_factored(a,s,h) -> Printf.sprintf
                               "(reduction %s capturing [%s] hiding %s)"
                               (String.concat "/" (List.map string_of_int a))
                               (String.concat ", " s)
                               (print_matchingtree h)
  | Match_ignore(a) -> Printf.sprintf "(ignore %s)" (print_matchingtree a)
  | Match_null -> "(null)"
  | Match_phi(s) -> Printf.sprintf "(phi/%s)" s

(* If an immediate of a particular flavour 'a' (already matched as a subtree)
   can overlap with a different flavour immediate 'b' (i.e., could possibly
   match as a subtree of the tree in question). *)
let imm_flavour_overlaps_p a b =
  match a with
    Shift_imm ->
      begin match b with
        Shift_imm
      | Data_imm
      | Maybenegdata_imm
      | El_imm
      | Eh_imm
      | Bm_imm
      | Hm_imm
      | Wm_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Data_imm ->
      begin match b with
        Shift_imm
      | Data_imm
      | Maybenegdata_imm
      | Eh_imm
      | El_imm
      | Wm_imm
      | Hm_imm
      | Bm_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Float_imm ->
      begin match b with
        Float_imm -> true
      | _ -> false
      end
  | Invdata_imm ->
      begin match b with
        Invdata_imm
      | Fh_imm
      | Fl_imm
      | Maybenegdata_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Maybenegdata_imm ->
      begin match b with
        Float_imm -> false
      | _ -> true
      end
  | Eh_imm ->
      begin match b with
        Eh_imm
      | Data_imm
      | Shift_imm
      | Maybenegdata_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | El_imm ->
      begin match b with
        El_imm
      | Shift_imm
      | Data_imm
      | Maybenegdata_imm
      | Wm_imm
      | Hm_imm
      | Bm_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Fh_imm ->
      begin match b with
        Fh_imm
      | Invdata_imm
      | Maybenegdata_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Fl_imm ->
      begin match b with
        Fl_imm
      | Invdata_imm
      | Maybenegdata_imm
      | Wm_imm
      | Hm_imm
      | Bm_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Wm_imm ->
      begin match b with
        Wm_imm
      | Shift_imm
      | Data_imm
      | Maybenegdata_imm
      | Hm_imm
      | Bm_imm
      | Fl_imm
      | El_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Hm_imm ->
      begin match b with
        Hm_imm
      | Shift_imm
      | Data_imm
      | Maybenegdata_imm
      | Bm_imm
      | Wm_imm
      | Fl_imm
      | El_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Bm_imm ->
      begin match b with
        Bm_imm
      | Hm_imm
      | Wm_imm
      | Shift_imm
      | Data_imm
      | Maybenegdata_imm
      | Fl_imm
      | El_imm
      | Vanilla_imm -> true
      | _ -> false
      end
  | Vanilla_imm ->
      begin match b with
        Float_imm -> false
      | _ -> true
      end

(* Should be "less than or equal", to cope with narrowing immediates etc. *)
let rec subtrees_wider_or_equal a b =
  match a,b with
    Match_int_reg _, Match_int_reg _ -> true
  | Match_ptr_reg _, Match_ptr_reg _ -> true
  | Match_float_reg _, Match_float_reg _ -> true
  | Match_int_dst_reg _, Match_int_dst_reg _ -> true
  | Match_ptr_dst_reg _, Match_ptr_dst_reg _ -> true
  | Match_float_dst_reg _, Match_float_dst_reg _ -> true
  | Match_imm(_,fx), Match_imm(_,fy) -> imm_flavour_overlaps_p fx fy
  | Match_const x, Match_const y when x=y -> true
  | Match_nop a, Match_nop b -> a=b
  | Match_unop(a,c1), Match_unop(b,d1) ->
      a=b && subtrees_wider_or_equal c1 d1
  | Match_binop(a,c1,c2), Match_binop(b,d1,d2) ->
      a=b && subtrees_wider_or_equal c1 d1
        && subtrees_wider_or_equal c2 d2
  | Match_triop(a,c1,c2,c3), Match_triop(b,d1,d2,d3) ->
      a=b && subtrees_wider_or_equal c1 d1
        && subtrees_wider_or_equal c2 d2 && subtrees_wider_or_equal c3 d3
  | Match_move(c1,c2), Match_move(d1,d2) ->
      subtrees_wider_or_equal c1 d1 && subtrees_wider_or_equal c2 d2
  | Match_gen_temp(_,c1), Match_gen_temp(_,d1) ->
      subtrees_wider_or_equal c1 d1
  | Match_gen_temp(_,c1), c2 ->
      subtrees_wider_or_equal c1 c2
  | c1, Match_gen_temp(_,c2) ->
      subtrees_wider_or_equal c1 c2
  | Match_factored(_,_,a), Match_factored(_,_,b) ->
      subtrees_wider_or_equal a b
  | Match_ignore a, Match_ignore b -> subtrees_wider_or_equal a b
  | Match_null, Match_null -> true
  | _ -> false

let in_list msubtree mtlist =
  let rec scan mtl =
    match mtl with
      [] -> None
    | (num,m)::ms ->
     (*   Printf.printf "checking trees: %s == %s\n"
          (print_matchingtree msubtree) (print_matchingtree m); *)
        if subtrees_wider_or_equal msubtree m then begin
          (* print_endline "yes"; *)
          Some num
        end else begin
          (*print_endline "no"; *)
          scan ms
        end
  in
    scan mtlist

let factored_away_vars tree =
  let rec scan subtree acc =
    match subtree with
      Match_nop(op) -> acc
    | Match_unop(op,c1) -> scan c1 acc
    | Match_binop(op,c1,c2) -> scan c1 (scan c2 acc)
    | Match_triop(op,c1,c2,c3) -> scan c1 (scan c2 (scan c3 acc))
    | Match_move(c1,c2) -> scan c1 (scan c2 acc)
    | Match_null -> acc
    | Match_gen_temp(t,c1) -> scan c1 (t::acc)
    | Match_ignore(t) -> scan t acc
    | Match_int_reg nam
    | Match_ptr_reg nam
    | Match_float_reg nam
    | Match_int_dst_reg nam
    | Match_ptr_dst_reg nam
    | Match_float_dst_reg nam
    | Match_imm(nam,_) -> nam::acc
    | Match_factored(_,sub,_) -> sub @ acc
    | Match_const _ -> acc
    | Match_phi _ -> acc
  in
    scan tree []

(* This should in particular *not* scan the "top-level" of each tree. *)
let rec match_factorise mtree mtlist =
  let factor f =
    let g = List.filter (fun (_,cf) -> subtrees_wider_or_equal cf f) mtlist in
    match g with
      [] -> f
    | ns -> Match_factored(List.map (fun q,_ -> q) ns, factored_away_vars f, f)
  in let rec scan m =
    match m with
      Match_unop(op,c1) ->
        Match_unop(op, factor (scan c1))
    | Match_binop(op,c1,c2) ->
        Match_binop(op, factor (scan c1), factor (scan c2))
    | Match_triop(op,c1,c2,c3) ->
        Match_triop(op, factor (scan c1), factor (scan c2), factor (scan c3))
    | Match_move(c1,c2) ->
        Match_move(factor (scan c1), factor (scan c2))
    | Match_gen_temp(t,c1) ->
        Match_gen_temp(t, scan c1)
    | Match_ignore(c) ->
        Match_ignore(factor (scan c))
    | x -> x
  in
    let num, mtree' = mtree in
    num, scan mtree'

let normalize mtlist =
  let rec retry curr prev =
    let fresh = List.map (fun m -> match_factorise m curr) curr in
    if fresh = curr then begin
      Printf.fprintf stderr "Stabilised.\n";
      let o = open_out "stable.rules" in
      List.iter
        (fun (num,rul) ->
          output_string o ((string_of_int num) ^ ": " ^
                           (print_matchingtree rul) ^ "\n"))
        fresh;
      close_out o;
      fresh
    end else begin
      Printf.fprintf stderr "Rerun normalize...\n";
      retry fresh curr
    end
  in
    retry mtlist []

(*  let rec scan mtree =
    let newsubtree =
      match mtree with
        Match_unop(op,c1) ->
          Match_unop(op, scan c1)
      | Match_binop(op,c1,c2) ->
          Match_binop(op, scan c1, scan c2)
      | Match_triop(op,c1,c2,c3) ->
          Match_triop(op, scan c1, scan c2, scan c3)
      | Match_move(c1,c2) ->
          Match_move(scan c1, scan c2)
      | Match_gen_temp(t,c1) ->
          Match_gen_temp(t, scan c1)
      | x -> x
    in
      match in_list newsubtree mtlist with
        None -> newsubtree
      | Some n ->
          begin match mtree with
            Match_factored(ns, fvars) ->
              let newfvars = factored_away_vars newsubtree in
              if newfvars <> fvars then
                failwith "Factored var list must be identical"
              else
                Match_factored(n::ns, fvars)
          | _ -> Match_factored([n], factored_away_vars newsubtree)
          end
  in let num, mtree' = mtree in
  num, scan mtree' *)

(* Applies 'f' to each member of ls and the remainder of ls, minus that
   element. I.e., for each member of ls, calculates

     f <element> <list of other elements>

   And builds a list of the results. Could probably be more efficient.
*)
let nonself f ls =
  let rec scan starts ends =
    match ends with
      [] -> []
    | e::es -> f e (starts @ es) :: scan (e :: starts) es
  in
    scan [] ls

let prec_patt = function
    Iformat.Single -> <:patt< Iformat.Single >>
  | Iformat.Double -> <:patt< Iformat.Double >>

let indir_patt = function
    Ast.Byte -> <:patt< Ast.Byte >>
  | Ast.Halfword -> <:patt< Ast.Halfword >>
  | Ast.Word -> <:patt< Ast.Word >>
  | Ast.Doubleword -> <:patt< Ast.Doubleword >>
  | Ast.WordVec -> <:patt< Ast.WordVec >>
  | Ast.DoublewordVec -> <:patt< Ast.DoublewordVec >>

let cond_patt = function
    Iformat.Eq -> <:patt< Iformat.Eq >>
  | Iformat.Ne -> <:patt< Iformat.Ne >>
  | Iformat.Ge -> <:patt< Iformat.Ge >>
  | Iformat.Geu -> <:patt< Iformat.Geu >>
  | Iformat.Gt -> <:patt< Iformat.Gt >>
  | Iformat.Gtu -> <:patt< Iformat.Gtu >>
  | Iformat.Le -> <:patt< Iformat.Le >>
  | Iformat.Leu -> <:patt< Iformat.Leu >>
  | Iformat.Lt -> <:patt< Iformat.Lt >>
  | Iformat.Ltu -> <:patt< Iformat.Ltu >>
  | Iformat.Andl -> <:patt< Iformat.Andl >>
  | Iformat.Eorl -> <:patt< Iformal.Eorl >>
  | Iformat.Nandl -> <:patt< Iformat.Nandl >>
  | Iformat.Neorl -> <:patt< Iformat.Neorl >>

let trap_patt = function
    Iformat.OnZero -> <:patt< Iformat.OnZero >>
  | Iformat.OnNonzero -> <:patt< Iformat.OnNonzero >>

(* Can we make a list of all results, then find the least element?
   Not with this technique, though we can 'cheat' and make all patterns
   guarded by a "current best guess", so only better rules will match.
   Then we iterate until we get the best result. Probably a bit inefficient,
   depends how well the patterns can be compiled by ocamlc.  *)

let rec match_build (ruleno, mt) =
  match mt with
    Match_int_reg n ->
      let pat = <:patt< (Register(_, (Id.IntType | Id.PtrType), _)
                        | Pseudo(_, (Id.IntType | Id.PtrType), _)
                        | Reduction {
                            reduction = Pseudo(_, (Id.IntType | Id.PtrType), _)
                          } as $lid:n$) >>
      and cc = <:expr< cost_int_node $lid:n$ >>,
               <:expr< kids_int_node $lid:n$ >> in
      pat, [], [cc]
  | Match_ptr_reg n ->
      let pat = <:patt< ((Register(_, Id.PtrType, _)
                        | Pseudo(_, Id.PtrType, _)
                        | Reduction {
                            reduction = Pseudo(_, Id.PtrType, _)
                          }) as $lid:n$) >>
      and cc = <:expr< cost_ptr_node $lid:n$ >>,
               <:expr< kids_ptr_node $lid:n$ >> in
      pat, [], [cc]
  | Match_float_reg n ->
      let pat = <:patt< ((Register(_, Id.FloatType, _)
                        | Pseudo(_, Id.FloatType, _)
                        | Reduction {
                            reduction = Pseudo(_, Id.FloatType, _)
                          }) as $lid:n$) >>
      and cc = <:expr< cost_float_node $lid:n$ >>,
               <:expr< kids_float_node $lid:n$ >> in
      pat, [], [cc]
  | Match_int_dst_reg n ->
      let pat = <:patt< ((Register(_, (Id.IntType | Id.PtrType), _)
                        | Pseudo(_, (Id.IntType | Id.PtrType), _))
                        as $lid:n$) >> in
      pat, [], []
  | Match_ptr_dst_reg n ->
      let pat = <:patt< ((Register(_, Id.PtrType, _)
                        | Pseudo(_, Id.PtrType, _))
                        as $lid:n$) >> in
      pat, [], []
  | Match_float_dst_reg n ->
      let pat = <:patt< ((Register(_, Id.FloatType, _)
                        | Pseudo(_, Id.FloatType, _))
                        as $lid:n$) >> in
      pat, [], []
  | Match_imm(n, Shift_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_shiftimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Data_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_dataimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Float_imm) ->
      let pat = <:patt< (Floatconst _ as $lid:n$) >>
      and grd = <:expr< match_floatimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Invdata_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_invdataimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Maybenegdata_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_maybenegdataimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Eh_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_ehimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, El_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_elimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Fh_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_fhimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Fl_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_flimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Wm_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_wmimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Hm_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_hmimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Bm_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
      and grd = <:expr< match_bmimm $lid:n$ >> in
      pat, [grd], []
  | Match_imm(n, Vanilla_imm) ->
      let pat = <:patt< (Constant _ as $lid:n$) >>
     (* and grd = <:expr< match_imm $lid:n$ >> *) in
      pat, [], []
  | Match_const cst ->
      let nam = name "n" in
      let cstr = Int32.to_string cst in
      let pat = <:patt< (Constant _ as $lid:nam$) >>
      and grd = <:expr< $lid:nam$ = Constant(Int32.of_string $str:cstr$) >> in
      pat, [grd], []
  | Match_nop(op) ->
      let op' = match op with
        Return -> <:patt< Return >>
      in let pat = <:patt< Zop($op'$) >> in
      pat, [], []
  | Match_null ->
      <:patt< Null >>, [], []
  | Match_unop(op,a) ->
      let op' =
        match op with
          Not -> <:patt< Not >>
        | Ind(sz) -> let sz' = indir_patt sz in <:patt< Ind($sz'$) >>
        | IndVol(sz) -> let sz' = indir_patt sz in <:patt< IndVol($sz'$) >>
        | Swi -> <:patt< Swi >>
        | Jump -> <:patt< Jump >>
        | Neg_f p -> let p = prec_patt p in <:patt< Neg_f $p$ >>
        | Abs_f p -> let p = prec_patt p in <:patt< Abs_f $p$ >>
        | Sqr_f p -> let p = prec_patt p in <:patt< Sqr_f $p$ >>
        | Cast_f p -> let p = prec_patt p in <:patt< Cast_f $p$ >>
      in let pa,gra,cc = match_build (ruleno,a) in
      let pat = <:patt< Unop($op'$, $pa$) >> in
      pat, gra, cc
  | Match_binop(op,a,b) ->
      let op' =
        match op with
          Add -> <:patt< Add >>
        | Sub -> <:patt< Sub >>
        | Mul -> <:patt< Mul >>
        | Div -> <:patt< Div >>
        | Lsl -> <:patt< Lsl >>
        | Lsr -> <:patt< Lsr >>
        | Asr -> <:patt< Asr >>
        | Ror -> <:patt< Ror >>
        | And -> <:patt< And >>
        | Ior -> <:patt< Ior >>
        | Eor -> <:patt< Eor >>
        | Cmp c -> let c = cond_patt c in <:patt< Cmp $c$ >>
        | Udiv -> <:patt< Udiv >>
        | Mod -> <:patt< Mod >>
        | Umod -> <:patt< Umod >>
        | Mlh -> <:patt< Mlh >>
        | Umlh -> <:patt< Umlh >>
        | Call -> <:patt< Call >>
        | Add_f p -> let p = prec_patt p in <:patt< Add_f $p$ >>
        | Sub_f p -> let p = prec_patt p in <:patt< Sub_f $p$ >>
        | Mul_f p -> let p = prec_patt p in <:patt< Mul_f $p$ >>
        | Div_f p -> let p = prec_patt p in <:patt< Div_f $p$ >>
        | Trap tc -> let tc = trap_patt tc in <:patt< Trap $tc$ >>
      in let pa,gra,cca = match_build (ruleno,a) in
      let pb,grb,ccb = match_build (ruleno,b) in
      let pat = <:patt< Binop($op'$, $pa$, $pb$) >> in
      pat, gra @ grb, cca @ ccb
  | Match_triop(op,a,b,c) ->
      let op' =
        match op with
          Bitfield -> <:patt< Bitfield >>
        | SignedBitfield -> <:patt< SignedBitfield >>
        | Branch -> <:patt< Branch >>
        | SpliceShift -> <:patt< SpliceShift >>
        | Splice -> <:patt< Splice >>
      in let pa,gra,cca = match_build (ruleno,a) in
      let pb,grb,ccb = match_build (ruleno,b) in
      let pc,grc,ccc = match_build (ruleno,c) in
      let pat = <:patt< Triop($op'$, $pa$, $pb$, $pc$) >> in
      pat, gra @ grb @ grc, cca @ ccb @ ccc
  | Match_factored(ns,fvar,hidden) ->
      let rule = name "reduct" in
      let rec any_of_these_nums ns =
        match ns with
          [] -> failwith "Empty num list"
        | [n] ->
            let sn = string_of_int n in
            <:patt< $int:sn$ >>
        | n::ns ->
            let sn = string_of_int n
            and rem = any_of_these_nums ns in
            <:patt< $int:sn$ | $rem$ >> in
      let nums = any_of_these_nums ns in
      let fkids_cost = name "fk" in
      let fvars = List.fold_right
        (fun x l -> <:patt< [$lid:x$ :: $l$] >>) fvar <:patt< [] >> in
      let pat = <:patt< Reduction { ruleno = $nums$;
                                    cost_with_kids = $lid:fkids_cost$;
                                    factored_vars = $fvars$
                                  } >> in
      let cc = <:expr< $lid:fkids_cost$ >>,
               <:expr< fun x -> x >> in
      let _, pulled_out_guards, _ = match_build (ruleno,hidden) in
      pat, pulled_out_guards, [cc]
  | Match_phi(args) ->
      let pat = <:patt< Phi($lid:args$) >> in
      pat, [], []
  | _ -> failwith "Bad match at nested level"

let lids_of_stringlist e =
  List.fold_right
    (fun i acc -> <:expr< [$lid:i$ :: $acc$] >>)
    e
    <:expr< [] >>

(* Warning: don't try putting Phi nodes in more complex expressions, it won't
   work.  *)
let suck_du_from_match mat =
  let rec scan defs uses puses = function
    Match_int_reg use
  | Match_ptr_reg use
  | Match_float_reg use -> defs, use::uses, puses
  | Match_int_dst_reg def
  | Match_ptr_dst_reg def
  | Match_float_dst_reg def -> def::defs, uses, puses
  | Match_imm _
  | Match_const _
  | Match_nop _
  | Match_null -> defs, uses, puses
  | Match_unop(op,x) -> scan defs uses puses x
  | Match_binop(op,x,y) ->
      let d1,u1,p1 = scan defs uses puses x in
      scan d1 u1 p1 y
  | Match_triop(op,x,y,z) ->
      let d1,u1,p1 = scan defs uses puses x in
      let d2,u2,p2 = scan d1 u1 p1 y in
      scan d2 u2 p2 z
  | Match_move(x,y) ->
      let d1,u1,p1 = scan defs uses puses x in
      scan d1 u1 p1 y
  | Match_ignore(x) -> scan defs uses puses x
  | Match_gen_temp(def,x) -> scan (def::defs) uses puses x
  | Match_phi(uses1) -> defs, uses, Some uses1
  | Match_factored(_,fvar,mt) ->
      scan defs uses puses mt
  in
    let d1,u1,p1 = scan [] [] None mat in
    match p1 with
      None -> lids_of_stringlist d1, lids_of_stringlist u1
    | Some p -> lids_of_stringlist d1,
                  <:expr< List.filter Ast.is_register (Array.to_list $lid:p$) >>

let match_build_toplevel (ruleno,mt) =
  let defs,uses = suck_du_from_match mt in
  match mt with
    Match_move(a,b) ->
      let pa,gra,cca = match_build (ruleno,a) in
      let pb,grb,ccb = match_build (ruleno,b) in
      let pat = <:patt< Move($pa$, $pb$) >> in
      fun asm ->
        let struleno = string_of_int ruleno in
        pat, gra @ grb, cca @ ccb,
        <:expr< Reduction { reduction = Null;
                            ruleno = $int:struleno$;
                            cost = tcosts;
                            cost_with_kids = tcosts - self_cost;
                            defs = $defs$;
                            uses = $uses$;
                            factored_vars = [];
                            insn = $asm$
                          } >>
  | Match_gen_temp(tmpnam, b) ->
      let pat,guards,costfn = match_build (ruleno,b) in
      let struleno = string_of_int ruleno in
      let fvar = factored_away_vars b in
      let fvars = List.fold_right
        (fun x l -> <:expr< [$lid:x$ :: $l$] >>) fvar <:expr< [] >> in
      fun asm ->
        let red =
          <:expr< Reduction { reduction = $lid:tmpnam$;
                              ruleno = $int:struleno$;
                              cost = tcosts;
                              cost_with_kids = tcosts - self_cost;
                              defs = $defs$;
                              uses = $uses$;
                              factored_vars = $fvars$;
                              insn = $asm$
                            } >> in
        let newtempexp =
          <:expr< let $lid:tmpnam$ = gen_int_pseudo () in $red$ >> in
        pat, guards, costfn, newtempexp
  | Match_ignore(stmt) ->
      let pat,guards,costfn = match_build (ruleno,stmt) in
      let struleno = string_of_int ruleno in
      fun asm ->
        pat, guards, costfn,
        <:expr< Reduction { reduction = Null;
                            ruleno = $int:struleno$;
                            cost = tcosts;
                            cost_with_kids = tcosts - self_cost;
                            defs = $defs$;
                            uses = $uses$;
                            factored_vars = [];
                            insn = $asm$
                          } >>
  | x -> failwith ("Bad match at top level: " ^ print_matchingtree x)

let map_with_num f src =
  let _,out =
    List.fold_left (fun (n,ls) i -> (n+1, f n i :: ls)) (0,[]) src
  in
    List.rev out

EXTEND
  GLOBAL: insn_match;
  
  insn_match:
    [[
      mz = match_insns ->
        let combineguards = function
          [] -> None
        | [sigl] -> Some <:expr< $sigl$ >>
        | conds ->
            let rec combine = function
              [] -> failwith "what?"
            | [c] -> <:expr< $c$ >>
            | c::cs -> let rest = combine cs in <:expr< $c$ && $rest$ >>
            in
              Some (combine conds)
        in let rec combinecosts base additional =
          match additional with
            [] ->
              <:expr< $base$ >>
          | (c,_)::cs ->
              let rest = combinecosts base cs in
              <:expr< $c$ + $rest$ >>
        in let rec combinekids additional =
          match additional with
            [] ->
              <:expr< [] >>
          | (_,k)::cs ->
              let rest = combinekids cs in
              <:expr< $k$ $rest$ >>
        in let rec makefun matches patterns num =
          match matches,patterns with
            [],[] -> []
          | (cost,_,asm)::ms, pat::pats ->
              let pat_minus_asm_closure = match_build_toplevel pat in
              let pat, guards, costfn, newast = pat_minus_asm_closure asm in
          (*    let numstr = string_of_int num in *)
              let cost_st = string_of_int cost in
              let cost_ex = <:expr< $int:cost_st$ >> in
              let costs = combinecosts cost_ex costfn in
              let kids = combinekids costfn in
              let guards =
                <:expr< $costs$ < least_cost >>
                :: guards in
              let clause = (pat,
                            combineguards guards,
                            <:expr< let tcosts = $costs$
                                    and tkids = $kids$
                                    and self_cost = $cost_ex$ in
                                    ($newast$, tkids) >>)
              in
                clause :: makefun ms pats (num+1)
          | _ -> failwith "Mismatched list lengths?"
        in
          let matches_only = map_with_num (fun n (_,x,_) -> n,x) mz in
          let factored_matches = normalize matches_only in
          let processed_pats = makefun mz factored_matches 0 in
          <:expr< fun least_cost -> fun [ $list:processed_pats$ ] >>
    ]];
  
  match_insns:
    [[
      insn = match_insn -> [insn]
    | insn = match_insn; EOL; insns = match_insns ->
        insn :: insns
    ]];
  
  match_insn:
    [[
      cs = cost; COLON; rl = rulebody; DEFINE; asm = assembler ->
        (cs, rl, asm)
     (*   let pattern, guards, temps = rl in
        
        in
          let rewrite_ast =
            match temps with
              None -> <:expr< src_ast >>
            | Some temp -> <:expr< Reduction($temp$, src_ast) >>
          in
            let rhs =
              <:expr< ( $int:cs$, $rewrite_ast$, fun alloc -> $asm$ ) >> in
            (pattern, combineguards guards, rhs) *)
    ]];
  
  cost:
    [[
      cs = DEC -> int_of_string cs
    ]];
  
  assembler:
    [[
      s = ASSEMBLER ->
        (* Probably doesn't handle error locations properly! *)
        let e = Grammar.Entry.parse Assemble.asm
                  (Stream.of_string (Asmlex.trim s)) in
        <:expr< Insn $anti:e$ >>
    | c = CAML ->
        let e = Grammar.Entry.parse Pcaml.expr
                  (Stream.of_string (Asmlex.trim c)) in
        <:expr< Insn $anti:e$ >>
    | "phi_passthrough"; "("; r = REG; ","; args = ID; ")" ->
        <:expr< Iphi($lid:r$, $lid:args$) >>
    ]];
  
  rulebody:
    [[
      lhs = lexpr; BECOMES; rhs = expr -> Match_move(lhs, rhs)
    | lhs = lfloatexpr; BECOMES; rhs = floatexpr -> Match_move(lhs, rhs)
    | lhs = mktmp; DECLARE; rhs = expr -> Match_gen_temp(lhs, rhs)
    | IGNORE; e = voidexpr -> Match_ignore(e)
    ]];
  
  reg:
    [[
      r = REG -> Match_int_reg r
    ]];
  
  freg:
    [[
      f = FREG -> Match_float_reg f
    ]];
  
  dreg:
    [[
      r = REG -> Match_int_dst_reg r
    ]];
  
  dfreg:
    [[
      f = FREG -> Match_float_dst_reg f
    ]];

  imm:
    [[
      SHIFTIMM; n = ALPHA -> Match_imm(n, Shift_imm)
    | DATAIMM; n = ALPHA -> Match_imm(n, Data_imm)
    | FLOATIMM; n = ALPHA -> Match_imm(n, Float_imm)
    | INVDATAIMM; n = ALPHA -> Match_imm(n, Invdata_imm)
    | MAYBENEGDATAIMM; n = ALPHA -> Match_imm(n, Maybenegdata_imm)
    | EHIMM; n = ALPHA -> Match_imm(n, Eh_imm)
    | ELIMM; n = ALPHA -> Match_imm(n, El_imm)
    | FHIMM; n = ALPHA -> Match_imm(n, Fh_imm)
    | FLIMM; n = ALPHA -> Match_imm(n, Fl_imm)
    | WORDMEMIMM; n = ALPHA -> Match_imm(n, Wm_imm)
    | HALFMEMIMM; n = ALPHA -> Match_imm(n, Hm_imm)
    | BYTEMEMIMM; n = ALPHA -> Match_imm(n, Bm_imm)
    | IMM; n = ALPHA -> Match_imm(n, Vanilla_imm)
    ]];
  
  const:
    [[
      n = NUM -> Match_const (Int32.of_string n)
    ]];
  
  expr:
    [[
      rm = expr; "lsl"; rn = expr -> Match_binop(Lsl, rm, rn)
    | rm = expr; "lsr"; rn = expr -> Match_binop(Lsr, rm, rn)
    | rm = expr; "asr"; rn = expr -> Match_binop(Asr, rm, rn)
    | rm = expr; "ror"; rn = expr -> Match_binop(Ror, rm, rn)
    | rm = expr; "&"; rn = expr -> Match_binop(And, rm, rn)
    | rm = expr; "|"; rn = expr -> Match_binop(Ior, rm, rn)
    | rm = expr; "^"; rn = expr -> Match_binop(Eor, rm, rn)
    | rm = expr; "+"; rn = expr -> Match_binop(Add, rm, rn)
    | rm = expr; "-"; rn = expr -> Match_binop(Sub, rm, rn)
    | rm = expr; "*"; rn = expr -> Match_binop(Mul, rm, rn)
    | rm = expr; "/"; rn = expr -> Match_binop(Div, rm, rn)
    | rm = expr; "%"; rn = expr -> Match_binop(Mod, rm, rn)
    | rm = expr; "/u"; rn = expr -> Match_binop(Udiv, rm, rn)
    | rm = expr; "%u"; rn = expr -> Match_binop(Umod, rm, rn)
    | rm = expr; "=="; rn = expr -> Match_binop(Cmp Iformat.Eq, rm, rn)
    | rm = expr; "!="; rn = expr -> Match_binop(Cmp Iformat.Ne, rm, rn)
    | rm = expr; "<="; rn = expr -> Match_binop(Cmp Iformat.Le, rm, rn)
    | rm = expr; "<"; rn = expr -> Match_binop(Cmp Iformat.Lt, rm, rn)
    | rm = expr; ">="; rn = expr -> Match_binop(Cmp Iformat.Ge, rm, rn)
    | rm = expr; ">"; rn = expr -> Match_binop(Cmp Iformat.Gt, rm, rn)
    | rm = expr; "<=u"; rn = expr -> Match_binop(Cmp Iformat.Leu, rm, rn)
    | rm = expr; "<u"; rn = expr -> Match_binop(Cmp Iformat.Ltu, rm, rn)
    | rm = expr; ">=u"; rn = expr -> Match_binop(Cmp Iformat.Geu, rm, rn)
    | rm = expr; ">u"; rn = expr -> Match_binop(Cmp Iformat.Gtu, rm, rn)
    | rm = reg -> rm
    | i = imm -> i
    | c = const -> c
    | "("; e = expr; ")" -> e
    | mm = indir; e = expr -> Match_unop(mm, e)
    | BITF; "("; bpl = imm; ","; bph = imm; ","; e = expr; ")" ->
        Match_triop(Bitfield, bpl, bph, e)
    | SBITF; "("; bpl = imm; ","; bph = imm; ","; e = expr; ")" ->
        Match_triop(SignedBitfield, bpl, bph, e)
    | "~"; rm = expr -> Match_unop(Not, rm)
    | "phi"; "("; args = ID; ")" -> Match_phi args
   (* | "["; rs = REG; DOTS; re = REG; "]" ->
        <:expr< (match_vector $str:rs$ $str:re$) >> *)
    ]];

  lexpr:
    [[
      rm = dreg -> rm
    | "("; e = lexpr; ")" -> e
    | mm = indir; e = expr -> Match_unop(mm, e)
    ]];

  mktmp:
    [[
      TEMP; "("; rt = REG; ")" -> rt
    ]];

  floatexpr:
    [[
      SGL; fm = freg -> Match_unop(Cast_f Iformat.Single, fm)
    | DBL; fm = freg -> Match_unop(Cast_f Iformat.Double, fm)
    | fm = floatexpr; "+s"; fn = floatexpr ->
        Match_binop(Add_f Iformat.Single, fm, fn)
    | fm = floatexpr; "+d"; fn = floatexpr ->
        Match_binop(Add_f Iformat.Double, fm, fn)
    | fm = floatexpr; "-s"; fn = floatexpr ->
        Match_binop(Sub_f Iformat.Single, fm, fn)
    | fm = floatexpr; "-d"; fn = floatexpr ->
        Match_binop(Sub_f Iformat.Double, fm, fn)
    | fm = floatexpr; "*s"; fn = floatexpr ->
        Match_binop(Mul_f Iformat.Single, fm, fn)
    | fm = floatexpr; "*d"; fn = floatexpr ->
        Match_binop(Mul_f Iformat.Double, fm, fn)
    | fm = floatexpr; "/s"; fn = floatexpr ->
        Match_binop(Div_f Iformat.Single, fm, fn)
    | fm = floatexpr; "/d"; fn = floatexpr ->
        Match_binop(Div_f Iformat.Double, fm, fn)
    | fm = freg -> fm
    | SGL; i = imm ->
        Match_unop(Cast_f Iformat.Single, i)
    | DBL; i = imm ->
        Match_unop(Cast_f Iformat.Double, i)
    | "("; e = floatexpr; ")" -> e
   (* | DBL; "["; fs = FREG; DOTS; fe = FREG; "]" ->
        <:expr< match_vectorf $str:fs$ $str:fe$ >> *)
    | mm = indirf; e = expr -> Match_unop(mm, e)
    ]];

  lfloatexpr:
    [[
      rm = dfreg -> rm
    | "("; e = lfloatexpr; ")" -> e
    | mm = indirf; e = expr -> Match_unop(mm, e)
    ]];

  termonly:
    [[
      rm = reg -> rm
    | i = imm -> i
    | c = const -> c
    ]];

  voidexpr:
    [[
      "cbr"; rc = expr; ","; rt = expr; ","; rf = expr ->
        Match_triop(Branch, rc, rt, rf)
    | "call"; rc = expr; ","; rr = expr ->
        Match_binop(Call, rc, rr)
    | "jump"; rj = expr ->
        Match_unop(Jump, rj)
    | "ret" ->
        Match_nop(Return)
    | "trapz"; rd = expr; ","; rh = termonly ->
        Match_binop(Trap Iformat.OnZero, rd, rh)
    | "trapn"; rd = expr; ","; rh = termonly ->
        Match_binop(Trap Iformat.OnNonzero, rd, rh)
    | "nop" ->
        Match_null
    ]];

  indir:
    [[
      WORD -> Ind(Word)
    | HALF -> Ind(Halfword)
    | BYTE -> Ind(Byte)
    ]];

  indirf:
    [[
      SWORD -> Ind(Word)
    | DWORD -> Ind(Doubleword)
    ]];
  
END
;;

let add_quot_rule rule name =
  let pat s = failwith "patt type not implemented" in
  let exp s = Grammar.Entry.parse insn_match (Stream.of_string s) in
    Quotation.add name (Quotation.ExAst (exp, pat))

let _ = add_quot_rule insn_match "insn_match"
