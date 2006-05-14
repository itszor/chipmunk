  bss
_smurf:
  reserve 256 
_smurf2:
  reserve 256

  global _pad
  global _hellow
  data
_pad:
  dcs "I ate a hamster\n"
  align 4
_hellow:
  dcs "Hello world\n"
  align 4
 
  global _main
 
  text

_memcopy:
  stm.d r63*,[~r54]
  cmp.eq r3,r2,#0
  cbr r3,_stop,_dontstop
_dontstop:
  mov r3,#0
_memcopyloop:
  ldr.b r4,[r1,r3]
  str.b ~r4,[r0,r3]
  add r3,~r3,#1
  ucmp.lt r4,r3,r2
  cbr ~r4,_memcopyloop,_stop
_stop:
  ldm.i r63*,[r54]
  ret
 
_main:
  stm.d r63*,[~r54,r8]
  mvc.l r8,#_foobar
  mvc.h r8,#_foobar
  mvc.l r0,#_hellow
  mvc.h r0,#_hellow
  call _writestring, _c2
_c2:
  ldr.w r1,[r8,#0]
  mvc.l r0,#_smurf2
  mvc.h r0,#_smurf2
  mvc.l r0,#_smurf 
  mvc.h r0,#_smurf
  mov r2,#256
  mov r8,r0
  call _memcopy, _c3
_c3:
  mov r0,~r8
  call _writestring, _c1
_c1:
  mov r0,#0
  ldm.i r63*,[r8,r54]
  ret
  
