open Seq
open X86support

let b = Array.create 16 0l;;

external readarray : int32 array -> int -> int32 = "int32_array_get"
external readarray_addr : unit -> int32 = "int32_array_get_addr"

external writearray : int32 array -> int -> int32 -> unit = "int32_array_put"
external writearray_addr : unit -> int32 = "int32_array_put_addr"

external copy_int32_addr : unit -> int32 = "copy_int32_addr"

external invoke1 : string -> int32 array -> unit = "invoke1"
;;

let dumparray x =
  for i = 0 to (Array.length x)-1 do
    Printf.fprintf stderr "Array[%d]=%ld\n" i (readarray x i);
  done
;;

(*dumparray b;;
Printf.fprintf stderr "\n";;*)

writearray b 5 15l;;

(*

*)

(* let ptr to local_roots be  eax
       first param be  ecx
       second param be edx
       ..              ebx
                       esi
       scratch edi (our code address)

  From caml/memory.h:

  struct caml__roots_block {
    struct caml__roots_block *next;
    long ntables;
    long nitems;
    value *tables [5];
  };
*)
let (>>) a b = lazy_append a b

let header =
  <:x86<
    push ebp;
    mov edi,[eax];
    sub esp, #36;
    mov dword [esp+32], eax;
    mov dword [esp], edi;
    mov dword [eax], esp;
  >>
;;

let header1 =
  header >>
  <:x86<
    mov dword [esp+4], #1;
    mov dword [esp+8], #1;
    mov dword [esp+12], ecx
  >>
;;

let header2 =
  header >>
  <:x86<
    mov dword [esp+4], #2;
    mov dword [esp+8], #1;
    mov dword [esp+12], ecx;
    mov dword [esp+16], edx
  >>
;;

let header3 =
  header >>
  <:x86<
    mov dword [esp+4], #3;
    mov dword [esp+8], #1;
    mov dword [esp+12], ecx;
    mov dword [esp+16], edx;
    mov dword [esp+20], ebx
  >>
;;

let header4 =
  header >>
  <:x86<
    mov dword [esp+4], #4;
    mov dword [esp+8], #1;
    mov dword [esp+12], ecx;
    mov dword [esp+16], edx;
    mov dword [esp+20], ebx;
    mov dword [esp+24], esi
  >>
;;

let footer locals =
  let locals = Int32.of_int locals in
  <:x86<
    add esp, $abs:locals$;
    mov eax, [esp+32];
    mov edi, [esp];
    mov dword [eax], edi;
    add esp, #36;
    pop ebp;
    ret
  >>
;;

(* local* expect eax to be *local_roots
   edi starts INVALID, but can be made equal to esp if called right after
   param*
 *)
let local1 =
  <:x86<
    mov edi, esp;
    sub esp, #36;
    mov dword [esp], edi;
    xor edi, edi;
    mov dword [esp+32], edi;
    mov dword [eax], esp;
    lea edi, [esp+32];
    mov dword [esp+4], #1;
    mov dword [esp+8], #1;
    mov dword [esp+12], edi
  >>
;;

let local2 =
  <:x86<
    mov edi, esp;
    sub esp, #40;
    mov dword [esp], edi;
    xor edi, edi;
    mov dword [esp+32], edi;
    mov dword [esp+36], edi;
    mov dword [eax], esp;
    lea edi, [esp+32];
    mov dword [esp+4], #2;
    mov dword [esp+8], #1;
    mov dword [esp+12], edi;
    add edi, #4;
    mov dword [esp+16], edi
  >>
;;

let local3 =
  <:x86<
    mov edi, esp;
    sub esp, #44;
    mov dword [esp], edi;
    xor edi, edi;
    mov dword [esp+32], edi;
    mov dword [esp+36], edi;
    mov dword [esp+40], edi;
    mov dword [eax], esp;
    lea edi, [esp+32];
    mov dword [esp+4], #3;
    mov dword [esp+8], #1;
    mov dword [esp+12], edi;
    add edi, #4;
    mov dword [esp+16], edi;
    add edi, #4
    mov dword [esp+20], edi
  >>
;;

let local4 =
  <:x86<
    mov edi, esp;
    sub esp, #48;
    mov dword [esp], edi;
    xor edi, edi;
    mov dword [esp+32], edi;
    mov dword [esp+36], edi;
    mov dword [esp+40], edi;
    mov dword [esp+44], edi;
    mov dword [eax], esp;
    lea edi, [esp+32];
    mov dword [esp+4], #4;
    mov dword [esp+8], #1;
    mov dword [esp+12], edi;
    add edi, #4;
    mov dword [esp+16], edi;
    add edi, #4;
    mov dword [esp+20], edi;
    add edi, #4;
    mov dword [esp+24], edi
  >>
;;

let local5 =
  <:x86<
    mov edi, esp;
    sub esp, #52;
    mov dword [esp], edi;
    xor edi, edi;
    mov dword [esp+32], edi;
    mov dword [esp+36], edi;
    mov dword [esp+40], edi;
    mov dword [esp+44], edi;
    mov dword [esp+48], edi;
    mov dword [eax], esp;
    lea edi, [esp+32];
    mov dword [esp+4], #5;
    mov dword [esp+8], #1;
    mov dword [esp+12], edi;
    add edi, #4;
    mov dword [esp+16], edi;
    add edi, #4;
    mov dword [esp+20], edi;
    add edi, #4;
    mov dword [esp+24], edi;
    add edi, #4;
    mov dword [esp+28], edi
  >>
;;

let code =
  let read_p = readarray_addr ()
  and write_p = writearray_addr ()
  and copyint32_p = copy_int32_addr () in
  <:x86<
    mov ebx,[ecx];
    push dword #9;
    push ebx;
    mov esi,$abs:read_p$;
    call dword esi;
    add esp,#8;
    mov eax,[eax+4];
    inc eax;
    push eax;
    mov esi,$abs:copyint32_p$;
    call dword esi;
    add esp,#4;
    push eax;
    push dword #9;
    push ebx;
    mov esi,$abs:write_p$;
    call dword esi;
    add esp,#12
  >>
;;

(*
let code = <:x86< ret >>;;
*)

let code =
  let adj = 52 in
    flatten (header1 >> local5 >> code >> (footer adj))
;;

for i=0 to (String.length code)-1 do
  Printf.fprintf stderr ".byte %d\n" (int_of_char code.[i])
done
;;

let _ = invoke1 code b
;;

(*dumparray b;;*)
