open Common
open Exec
open State
(* open Disassemble *)
open I32op

(* Not quite beautiful... *)
module ParseArgs =
struct
  let anonarglist = ref []
  and progname = ref ""

  let anonarg x =
    match !progname with
      "" -> progname := x; anonarglist := x :: !anonarglist
    | _ -> anonarglist := x :: !anonarglist

  let trace = ref false
  and dumptrace = ref false
  and dumpread = ref false
  and dumpwrite = ref false
  and doregression = ref false
  and dokilltranslator = ref false
  and dooptimisation = ref false
  and optlimit = ref None
  ;;

  Arg.parse [("-t",
              Arg.Set(trace),
              "Print disassembly of execution trace");
             ("-d",
              Arg.Set(dumptrace),
              "Write block trace to trace.out");
             ("-r",
              Arg.Set(dumpread),
              "Write memory load trace to read.out");
             ("-w",
              Arg.Set(dumpwrite),
              "Write memory store trace to write.out");
             ("-R",
              Arg.Set(doregression),
              "Perform regression checking on optimisations");
             ("-k",
              Arg.Set(dokilltranslator),
              "Throw random numbers at machine, see what happens");
             ("-O",
              Arg.Set(dooptimisation),
              "Enable optimisation");
             ("-l",
              Arg.Int (fun n -> optlimit := Some n),
              "Limit optimisation to first n blocks only");
             ("--",
              Arg.Rest(anonarg),
              "Pass remaining args to simulated program unaltered")]
            anonarg
            "Usage: sim [-tdRrw] prog <args>"
  ;;
  
  let argvbase = 0x80000800l
  and argvstrings = ref 0x80001000l
  
  let injectargs state args =
    let mem = state#getmem in
    let rec injectarg args num =
      match args with
        [] -> ()
      | x::xs ->
          Disassemble.vmputstring state !argvstrings x;
          mem#writeWord (argvbase +! (!!num *! 4l)) !argvstrings;
          argvstrings := !argvstrings +! ((++!) !!(String.length x));
          injectarg xs (num+1)
    in
      injectarg (List.rev args) 0
  ;;
end

exception What;;
Random.self_init ();;

module RunMe =
struct
  let mach = new machine in
  let mem = mach#getmem
  and proc = mach#getproc in
  Callback.register "write word" mem#writeWord;
  let start = Loadelf.load_elf !ParseArgs.progname in
  Loadelf.clearbss mach;

(*  let dopt = new Client.dopgateway processor
  ;;*)
  
  (*for n = 0 to 100 do
    dopt#grindblock (Int32.of_int (n*16))
  done
  ;;*)
  
(*  let clientchan = Client.clientchannel () in
  Client.populateclient clientchan;
  clientchan#listen true; 

  let q = Channel.WrapInt 5 in
  let Channel.WrapInt(res) = clientchan#call "fargle" q in
  Printf.fprintf stderr "fargle %d = %d\n" 5 res; *)

  ParseArgs.injectargs mach !ParseArgs.anonarglist;

  proc#putreg 0 (!!(List.length !ParseArgs.anonarglist));
  proc#putreg 1 ParseArgs.argvbase;

  proc#putnameseg (mem#readWord Loadelf.namebase);
  mach#getserv#getindexer#setbase (mem#readWord Loadelf.lastidx);
  Loadelf.makecanonicalnames mach (mem#readWord Loadelf.namebase);

  mach#getserv#setoptlimit !ParseArgs.optlimit;
  mach#getserv#optimiser !ParseArgs.dooptimisation;

  (*
  (* test block name functionality *)
  for n = 0 to 100 do
    let ls = !! n in
    Printf.printf "Blockname %ld = %s\n" ls (Disassemble.blockname processor ls)
  done
  ;;
  *)

  (*Printf.printf "Start is at %lx\n" start
  ;;*)
  proc#putpc (Lookup.lookup mem proc start);
  proc#putreg 55 start;

(*  if !ParseArgs.dumptrace then proc#blocktrace#start "trace.out";
  if !ParseArgs.dumpread then proc#readtrace#start "read.out";
  if !ParseArgs.dumpwrite then proc#writetrace#start "write.out";*)

  if !ParseArgs.trace then proc#insntrace#start "insntrace.out";

  (*Printf.printf "Running code at %lx\n" processor.program_counter
  ;;*)

  flush stdout;

  if !ParseArgs.dokilltranslator then begin
    (* while true do
      let num = Int32.logor (Random.int32 65536l)
                            (Int32.shift_left (Random.int32 65536l) 16) in
      try
      let dec = Decode.decode num :: [Iformat.RetF] in
      let trans = Translate.translist dec in
      let merged = Mergeldt.mergeldt trans in
      let trans = Mergetransfer.mergetransfer merged in
      let selected = List.flatten (List.map (Select.select Rules.rules) trans)
      in let encoded = Encode.encodelist selected in
      match encoded with
        [] -> raise What
      | first::rest ->
          let newdec = Decode.decode first in
          let olddis = Disassemble.diss mach (Decode.decode num)
          and newdis = Disassemble.diss mach newdec in
          if olddis <> newdis then begin
            Printf.printf "values unequal.\n";
            Printf.printf "%s   vs\n%s\n" olddis newdis
          end
      with (Invalid_argument x) -> ()
          | Select.SoImGivingUp -> ()
          | Mergeldt.MissingLdt -> ()
          | Match_failure x -> ()
    done *)
    ()
  end;

  try
    if !ParseArgs.doregression then
      Regress.run mach
    else begin
      if mach#getproc#insntrace#active then
        Pipeline.run_with_trace mach
      else
        Pipeline.run mach
     end
   with Memory.UnalignedAccess x ->
     Printf.fprintf stderr "Unaligned access at %lx\n" x
end
