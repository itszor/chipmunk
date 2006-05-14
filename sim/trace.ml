class trace = object (self)
  val mutable logfile = stderr
  val mutable tracing = false
  
  method start file =
    logfile <- open_out file;
    tracing <- true
  
  method stop =
    tracing <- false
  
  method put line =
    if tracing then
      output_string logfile line
  
  method active = tracing
end
