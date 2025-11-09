structure BasicExn =
struct
   exception Syntax of string
   exception Bug of string
   exception RetGosub
   exception NextFor
   exception ForNext
   exception NoLine
   exception Direct
   exception Interrupt
   exception Quit
   exception Input
end

fun prErr s = (TextIO.output (TextIO.stdErr, s ^ "\n"); TextIO.flushOut TextIO.stdErr)
