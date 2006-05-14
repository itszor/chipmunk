  global _writestring
  global _foobar

  data
_foobar:
  dcw _pad
_mumble:
  dcw _hellow
  align 4

  text

// r0: pointer to null-terminated string
_writestring:
  stm.d r63*,[~r54,~r8]
  
_loop:
  ldr.b r1,[r0, #0]
  cmp.eq r2, r1, #0
  cbr r2, _stop, _outputchar
_outputchar:
  mov r8, ~r0
  mov r0, ~r1
  call __serial_output_byte, _c1
_c1:
  add r0, ~r8, #1
  jump _loop
  
_stop:
  ldm.i r63*,[r8,r54]
  ret
  
