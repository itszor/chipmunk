open Common
open Int32
open Bigarray

exception UnalignedAccess of int32

type chunk = ChunkEmpty
           | ChunkFull of (int32, int32_elt, c_layout) Array1.t
           | ChunkIO of (int -> int32) * (int -> int32 -> unit)

let splitaddress address =
  (to_int (shift_right_logical address 20), ((to_int address) land 0xfffff)/4)

let newchunk () = Array1.create Bigarray.int32 Bigarray.c_layout (256*1024)

let mask = lognot (of_int 3)

class memory =
  object (self)
    val memory = Array.make 4096 ChunkEmpty

    initializer
      memory.(3840) <- ChunkIO(Serial.serialread, Serial.serialwrite)

    method writeWord address value =
      let (chunkno, index) = splitaddress address in
      if ((to_int address) land 3)>0 then
        raise (UnalignedAccess address)
      else match memory.(chunkno) with
        ChunkEmpty ->
          let array = newchunk () in
            memory.(chunkno) <- ChunkFull(array);
            array.{index} <- value
      | ChunkFull(array) ->
    (*    Printf.printf "Writing %lx at %d:%d\n" value chunkno index;*)
          array.{index} <- value
      | ChunkIO(r, w) -> w index value

    method readWord address =
    (*  if address>=(Int32.of_string "0x10000000") &&
         address<(Int32.of_string "0x20000000")
      then begin
        state.readtrace#put (Printf.sprintf "%ld\n" address)
      end; *)
      let (chunkno, index) = splitaddress address in
      if ((to_int address) land 3)>0 then
        raise (UnalignedAccess address)
      else match memory.(chunkno) with
        ChunkEmpty ->
          let array = newchunk () in
            memory.(chunkno) <- ChunkFull(array);
        (* Printf.printf "Reading %lx from %d:%d\n"
           array.{index} chunkno index;*)
            array.{index}
      | ChunkFull(array) ->
     (*   Printf.printf "Reading %lx from %d:%d\n" array.{index} chunkno 
          index;*)
          array.{index}
      | ChunkIO(r, w) -> r index

    method writeByte address' value =
      let address = to_int address'
      and maddr = (logand address' mask) in
      let current = self#readWord maddr
      and byte = (address land 3)*8 in
      let newbits = insertbits current byte (byte+7) value in
    (*    Printf.printf "oldbits %lx, newbits %lx\n" current newbits;*)
        self#writeWord maddr newbits

    method writeHalfword address' value =
      let address = to_int address'
      and maddr = (logand address' mask) in
      let current = self#readWord maddr
      and halfword = (address land 2)*8
      in
        self#writeWord maddr (insertbits current halfword (halfword+15) value)

    method readByte address' =
      let address = to_int address'
      and maddr = (logand address' mask) in
      let res = logand (shift_right_logical (self#readWord maddr)
                                            ((address land 3)*8))
                       (of_int 0xff)
      in
     (*   Printf.printf "Read %ld from %lx\n" res address'; *)
        res

    method readHalfword address' =
      let address = to_int address'
      and maddr = (logand address' mask) in
      logand (shift_right_logical (self#readWord maddr)
               ((address land 2)*8))
             (of_int 0xffff)

end
