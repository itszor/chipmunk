open Int32

(* we sometimes need 32-bit left-shift *)
let mylsl value by =
  if by>31 then zero else shift_left value by

let insertbits into startbit endbit from =
  let mask = sub (mylsl one (endbit-startbit+1)) one
  in
  let shiftedmask = shift_left mask startbit
  in
  let hole = logand into (lognot shiftedmask)
  and shiftedmaskedfrom = shift_left (logand from mask) startbit
  in
    logor shiftedmaskedfrom hole

let truth instr bit = to_int
  (logand one (shift_right_logical instr bit)) != 0

let extractbits word lo hi sign =
  let mask = (sub (mylsl one (hi-lo+1)) one)
  in
    if (sign && truth word hi) then
      logor (lognot mask) (logand (shift_right_logical word lo) mask)
    else
      logand (shift_right_logical word lo) mask

(* if we don't care about sign extension *)
let bits word lo hi = extractbits word lo hi false

let latch holder newval =
  let temp = !holder in
    holder := newval;
    temp

let (++) a b = b a

let promote a b =
  let a64 = Int64.of_int32 a
  and b64 = Int64.of_int32 b in
  let a64' = if a64<Int64.zero then Int64.add 0x100000000L a64 else a64
  and b64' = if b64<Int64.zero then Int64.add 0x100000000L b64 else b64 in
  (a64', b64')

let uRem a b =
  let a', b' = promote a b in
  let rem = Int64.rem a' b' in
  Int64.to_int32 rem

let uDiv a b =
  let a', b' = promote a b in
  let r64 = Int64.div a' b' in
  Int64.to_int32 r64

let rec from_to f a n m =
  if n>m then
    a
  else
    let a' = f n a in from_to f a' (n+1) m
