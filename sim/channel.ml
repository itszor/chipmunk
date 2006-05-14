(* Two-way RPC communication channel thingy.
 * We can call a function named by a string
 * We pass one arguments using marshalling
 * We get one result back
 *)

(* We can't have a table of differently-shaped functions, so
 * do ugly wrapping thing instead. Will preserve type over
 * marshalling this way too, sort of.
 *)
type wrapper = WrapInt of int
             | WrapString of string
             | WrapIntList of int list
             | WrapInt32 of int32
             | WrapInt32List of int32 list

(* Our dynamic types can fail at runtime! *)
exception TypeError

class channel (ic:in_channel) (oc:out_channel) =
  object (self)
  
  val ic = ic
  val oc = oc
  val funs = Hashtbl.create 17
  val inprogress = Mutex.create ()
  val mutable currentid = 0
  
  method makelocal (name:string) (fn:wrapper -> wrapper) =
    Hashtbl.add funs name fn
  
  method makeid =
    currentid <- currentid+1;
    currentid
  
  method waitforinput =
    ignore (UnixLabels.select
      ~read:[Unix.descr_of_in_channel ic]
      ~write:[]
      ~except:[]
      ~timeout:5.0)
  
  (* Make outgoing function call *)
  method call name (args:wrapper) =
    let id = self#makeid in
    Mutex.lock inprogress;
    output_string oc (name ^ "\n");
    output_string oc ((string_of_int id) ^ "\n");
    Marshal.to_channel oc args [];
    flush oc;
    Mutex.unlock inprogress;
    self#waitforinput;
    let res = (Marshal.from_channel ic : wrapper) in
    res

  (* Listen for incoming function calls *)
  method listen newthread =
    let listener () =
      while true do
        self#waitforinput;
        Mutex.lock inprogress;
        let fnname = input_line ic in
        let id = int_of_string (input_line ic) in
        let args = Marshal.from_channel ic in
        let fn = Hashtbl.find funs fnname in
        let res = fn args in
        output_string oc ((string_of_int id) ^ "\n");
        Marshal.to_channel oc res [];
        flush oc;
        Mutex.unlock inprogress;
      done
    in
      if newthread then
        ignore (Thread.create listener ())
      else
        listener ()
      
  end
