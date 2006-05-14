open Common
open Memory

type entry = Empty
           | Counter of int32 * int32 * int ref
           | Saturated

exception SomeMistake
exception Trigger of int32 * int32

let defaulthash mask x y =
  Int32.to_int (Int32.logand (Int32.logxor (Int32.shift_left x 2) y)
                             (Int32.of_int mask))

class tcache = fun size hash ->
  object (self)
    val mutable cache = Array.create size (ref Empty)
    val mutable trigger = 16

    initializer
      (* Otherwise all array elements point to the same value, erm *)
      self#reset

    method reset =
      for ii = 0 to size-1 do
        cache.(ii) <- ref Empty
      done

    method trans fromblock toblock =
      let entry = cache.(hash fromblock toblock) in
      match !entry with
        Empty
      | Saturated -> self#first entry fromblock toblock
      | Counter(pfrom, pto, count) ->
          if pfrom=fromblock && pto=toblock then
            self#inc entry fromblock toblock
          else
            self#first entry fromblock toblock

    method private first entry fromblock toblock =
      entry := Counter(fromblock, toblock, ref 1)

    method private inc entry fromblock toblock =
      match !entry with
        Empty -> raise SomeMistake
      | Counter(f,t,c) when f=fromblock && t=toblock ->
          c := !c + 1;
          if !c >= trigger then begin
            entry := Saturated;
            raise (Trigger (fromblock, toblock))
          end
      | Counter(f,t,c) ->
          self#first entry fromblock toblock
      | Saturated -> ()

    method values = cache
    
    method settrigger t =
      trigger <- t

  end
