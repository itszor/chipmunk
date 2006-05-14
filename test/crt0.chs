global __start
global __exit

  text

__start:
  // set up stack pointer
  mvc.eh r63,#0x80000000
  sub r63,r63,#4

  // set up compiler-controlled memory pointer
  mvc.eh r61,#0x78000000
  sub r61,r61,#4

  call _main, __exit

__exit:
  swi 0
  jump __exit

