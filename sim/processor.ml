open Common
open I32op

class processor =
  object (self)
    val registers = Array.make 64 Int32.zero
    val oldregs = Array.make 64 Int32.zero
    val floatregs = Array.make 64 0.0
    val oldfloats = Array.make 64 0.0
    val mutable x_reg = Int32.zero
    val mutable y_reg = Int32.zero
    val mutable z_reg = Int32.zero
    val mutable c_reg = 0
    val flatch = latch (ref (Int32.shift_left 60l 26))
    val dlatch = latch (ref (Iformat.NoopF))
    val mutable program_counter = Int32.zero
    val mutable indirection_seg = Int32.zero
    val mutable program_seg = 0x10000000l
    val mutable name_seg = Int32.zero
    val mutable time = Int64.zero
    val mutable stalled = false
    val insntrace = new Trace.trace
    val blocktrace = new Trace.trace
    val readtrace = new Trace.trace
    val writetrace = new Trace.trace
  
    method getpc = program_counter
    
    method putpc x =
      program_counter <- x
    
    method getindirseg = indirection_seg
    method getnameseg = name_seg
    method getprogseg = program_seg

    method putnameseg x =
      name_seg <- x

    method getreg n = registers.(n)

    method putreg n v =
      registers.(n) <- v
    
    method getfreg n = floatregs.(n)
    
    method putfreg n v =
      floatregs.(n) <- v
    
    method rdiff n = registers.(n) <> oldregs.(n)
    
    method fdiff n = floatregs.(n) <> oldfloats.(n)
    
    method syncregs =
      Array.blit registers 0 oldregs 0 64;
      Array.blit floatregs 0 oldfloats 0 64
    
    method getx = x_reg
    method gety = y_reg
    method getz = z_reg
    
    method putx q =
      x_reg <- q
    
    method puty q =
      y_reg <- q
    
    method putz q =
      z_reg <- q
    
    method getblktrace = blocktrace
    
    method insntrace = insntrace
    
    method pipeflush =
      let nop = Iformat.NoopF in
      ignore (dlatch nop);
      ignore (flatch (Int32.shift_left 60l 26))
    
    method incrpc =
      program_counter <- Int32.add program_counter 4l
    
    method getflatch = flatch
    method getdlatch = dlatch
    
    method bumptime =
      time <- Int64.succ time
    
    method gettime = time
  end
