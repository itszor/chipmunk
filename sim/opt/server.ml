open Genlex

let send oc lin =
  Printf.fprintf oc lin;
  flush oc

let request oc blk =
(*  Printf.fprintf stderr "Request block %lx\n" blk;*)
  Printf.fprintf oc "getblk \"%lx\"\n" blk;
  flush oc

let request_hdr oc blk =
  Printf.fprintf oc "gethdr \"%lx\"\n" blk;
  flush oc

(* let lexer = make_lexer ["req"; "start"; "end"; "done"]

let hex2int32 s = Scanf.sscanf s "%lx" (fun n -> n)

let rec parse_blk oc = parser
    [< 'Kwd "start"; words = parse_words oc; rest >] -> words,rest
and parse_words oc = parser
    [< 'Kwd "end"; >] -> send oc "ok\n"; []
  | [< 'String word; r = parse_words oc >] ->
        Decode.decode (hex2int32 word) :: r

let rec parse_hdr oc = parser
    [< 'Kwd "start"; words = parse_hdrwords oc; rest >] ->
        Index.index_of_list words,rest
and parse_hdrwords oc = parser
    [< 'String word; r = parse_hdrwords oc >] ->
        (hex2int32 word) :: r

let rec parse_cmds oc = parser
    [< 'Kwd "req"; blkno = parse_blkref; theblock >] ->
        request oc blkno;
        let words,rest = parse_blk oc theblock in
        let replwords = Driver.driver words in
        Disast.writeblk replwords;
        request_hdr oc blkno;
        let oldhdr = parse_hdr oc rest in
        ignore oldhdr
and parse_blkref = parser
    [< 'String blkref >] ->
        hex2int32 blkref

let action f t =
  let rd,wrt,exc = UnixLabels.select
    ~read:[Unix.descr_of_in_channel f] ~write:[] ~except:[] ~timeout:-1.0
  in
    parse_cmds t (lexer (Stream.of_channel f))
   (* while true do
      print_endline (input_line f)
    done *)


let _ =
  let my_address = get_my_addr () in
  UnixLabels.establish_server action ~addr:(Unix.ADDR_INET(my_address,4848)) *)

let get_my_addr () =
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

module Funs = struct
  open Channel
  
  let fargle chan x =
    match x with
      WrapInt x -> (* chan#call "add" *) (WrapInt (x+1))
    | _ -> raise TypeError

end

let populateserver chan =
  chan#makelocal "fargle" (Funs.fargle chan)

let action ic oc =
  let servlet = new Channel.channel ic oc in
  populateserver servlet;
  servlet#listen false

let serverchannel () =
  let my_address = get_my_addr () in
  UnixLabels.establish_server action ~addr:(Unix.ADDR_INET(my_address, 4848))

let _ = serverchannel ()
