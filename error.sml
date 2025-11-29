structure BasicExn =
struct
   exception Syntax of string
   exception Bug of string
   exception Interrupt
   exception Quit
   exception Input
   exception Runtime of string
end

fun prErr s = (TextIO.output (TextIO.stdErr, s ^ "\n"); TextIO.flushOut TextIO.stdErr)
