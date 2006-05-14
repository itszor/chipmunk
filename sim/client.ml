open Common
open Genlex
open I32op

let get_my_addr () =
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

(* 53 through 56 are block-termination instructions *)

let sendblk mem proc oc blk =
  let rec uptoterm addr =
    let raw = mem#readWord addr in
    let opcode = Int32.to_int (Int32.shift_right_logical raw 26) in
    Printf.fprintf oc "\"%lx\"\n" raw;
    if opcode<53 || opcode>56 then uptoterm (addr +! 4l)
  in
  let base = (Lookup.lookup mem proc blk) in
  Printf.fprintf oc "start\n";
  uptoterm base;
  Printf.fprintf oc "end\n";
  flush oc

let sendref mem proc oc refno =
  let base = proc#getindirseg +! refno in
  let addr = ref base in
  for i=0 to 3 do
    let mem = mem#readWord !addr in
    Printf.fprintf oc "\"%lx\"\n" mem;
    addr := !addr +! 4l;
  done

let injectref mem proc words refno =
  let base = proc#getindirseg +! refno in
  let rec put words at =
    match words with
      [] -> ()
    | w::ws -> mem#writeWord at w; put ws (at +! 4l)
  in
    put words base

let injectblk mem proc words blk =
  let base = (Lookup.lookup mem proc blk) in
  let rec put words at =
    match words with
      [] -> ()
    | w::ws -> mem#writeWord at w; put ws (at +! 4l)
  in
    put words base

(*

let lexer = make_lexer ["getblk"; "putblk"; "getref"; "putref";
                        "start"; "end"; "done"]

 let hex2int32 s = Scanf.sscanf s "%lx" (fun n -> n)

let rec parse_cmds mem proc oc = parser
    [< 'Kwd "getblk"; blk = parse_blkref; parse_cmds >] -> sendblk mem proc oc blk
  | [< 'Kwd "putblk"; blk = parse_incoming; parse_cmds >] -> ()
  | [< 'Kwd "getref"; blk = parse_blkref; parse_cmds >] -> sendref mem proc oc blk
  | [< 'Kwd "putref"; blk = parse_incomref; parse_cmds >] -> ()
  | [< 'Kwd "ok"; _ = Stream.empty >] -> ()
and parse_blkref = parser
    [< 'String blkref >] -> Int32.of_string ("0x" ^ blkref)
and parse_incoming = parser
    [< 'Kwd "start"; words = parse_words >] -> words
and parse_words = parser
    [< 'Kwd "end" >] -> []
  | [< 'String word; r = parse_words >] -> (hex2int32 word) :: r
and parse_incomref = parser
    [< 'Kwd "start"; words = parse_refwords >] -> words
and parse_refwords = parser
    [< 'Kwd "end" >] -> []
  | [< 'String word; r = parse_refwords >] -> (hex2int32 word) :: r

let grindblock mem proc blk =
  let my_address = get_my_addr () in
  let ic,oc = (Unix.open_connection (Unix.ADDR_INET(my_address,4848))) in
  Printf.fprintf oc "req \"%lx\"" blk;
  flush oc;
  parse_cmds mem proc oc (lexer (Stream.of_channel ic));
  flush oc;
  Unix.shutdown_connection ic

*)

module Funs = struct
  open Channel
    
  let add x =
    match x with
      WrapInt x -> WrapInt (x+1)
    | _ -> raise TypeError

end

let clientchannel () =
  let my_address = get_my_addr () in
  let ic,oc = (Unix.open_connection (Unix.ADDR_INET(my_address, 4848))) in
  new Channel.channel ic oc

let populateclient chan =
  chan#makelocal "add" Funs.add
