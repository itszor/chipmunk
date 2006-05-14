
  global __serial_output_byte
  global __serial_input_queue
  global __serial_input_byte

  text

__serial_output_byte:
  mvc.eh r1,#0xf0000000
  str.wv r0,[r1,#0]
  ret
  
__serial_input_queue:
  mvc.eh r1,#0xf0000000
  ldr.wv r0,[r1,#4]
  ret

__serial_input_byte:
  mvc.eh r1,#0xf0000000
  ldr.wv r0,[r1,#8]
  ret


