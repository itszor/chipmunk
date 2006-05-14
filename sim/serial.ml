open Common

let serialread offset =
  match offset with
    4 -> Int32.zero
  | 8 -> Int32.zero
  | _ -> Int32.zero

let serialwrite offset value =
  match offset with
    0 -> print_char (char_of_int (Int32.to_int value)); flush stdout
  | _ -> ()
