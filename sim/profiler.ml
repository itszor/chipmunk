exception NotOptimised

class profiler =
  object (self)
    val hash = Hashtbl.create 251
    val dirs = Hashtbl.create 251
    
    method count (from:int32) (toblk:int32) =
      if Hashtbl.mem hash (from,toblk) then begin
        let q = Hashtbl.find hash (from,toblk) in
        if !q > 100 then begin
          Printf.fprintf stderr "Found more than 100 %ld->%ld\n"
            (Int32.div from 16l)
            (Int32.div toblk 16l);
          q := -1;
          true
        end else begin
          if !q >= 0 then
            q := !q+1
          else
            q := !q-1;
          false
        end
      end else begin
        Hashtbl.add hash (from,toblk) (ref 0);
        false
      end
    
    (* only call for cbr *)
    method count_cbr (blk:int32) (exitdir:Ast.ldtype) =
      let bump toref dir =
        match dir with
          Ast.X -> if !toref > -8 then decr toref
        | Ast.Y -> if !toref < 8 then incr toref
        | _ -> failwith "Count_cbr with Z"
      in try
        let entry = Hashtbl.find dirs blk in
        bump entry exitdir
      with Not_found ->
        let entry = ref 0 in
        bump entry exitdir;
        Hashtbl.add dirs blk entry
    
    (* Returns a float rather than the internal representation.
       0 means true taken very much
       1 means false taken very much
       0.5 means true & false taken equally
    *)
    method get_cbr (blk:int32) =
      try
        let entry = Hashtbl.find dirs blk in
        (float_of_int (!entry + 8)) /. 16.0
      with Not_found -> 0.5  (* No info, guess at unbiased *)
    
    method getrep from toblk =
      let q = Hashtbl.find hash (from,toblk) in
      if !q < 0 then -(!q) else raise NotOptimised
  
    method dumprecur =
      let print_item (f,t) v =
        if !v<0 then Printf.fprintf stderr "%ld->%ld: %d\n" f t (- !v)
      in
      Hashtbl.iter print_item hash
  
  end
