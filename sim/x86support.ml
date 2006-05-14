open Seq

type code = Byte of int
          | Bytes of int list
          | Rel32 of int32
          | Rel8 of int32
          | Abs32 of int32
          | Abs8 of int32
          | Label of string


let split_int32 n =
  let b0 = Int32.logand n 255l
  and b1 = Int32.logand (Int32.shift_right_logical n 8) 255l
  and b2 = Int32.logand (Int32.shift_right_logical n 16) 255l
  and b3 = Int32.logand (Int32.shift_right_logical n 24) 255l
  in
    [b0; b1; b2; b3]

let strint x =
  let q = String.create 1 in
  q.[0] <- char_of_int x;
  q

let flatten stream =
  let buf = Buffer.create 4 in
  let rec add = function
    Empty -> ()
  | Cons(l,ls) ->
      begin match l with
        Byte n -> Buffer.add_string buf (strint n)
      | Bytes ns -> List.iter (fun n -> Buffer.add_string buf (strint n)) ns
      | Rel32 r
      | Abs32 r ->
        List.iter (fun n -> Buffer.add_string buf
                             (strint (Int32.to_int n)))
        (split_int32 r)
      | Rel8 r
      | Abs8 r -> Buffer.add_string buf (strint (Int32.to_int r))
      | Label l -> ()
      end;
      add (Lazy.force ls)
  in
    add stream;
    Buffer.contents buf
