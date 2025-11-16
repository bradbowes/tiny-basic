structure BasicExn =
struct
   exception Syntax of string
   exception Bug of string
   exception RetGosub
   exception NextFor
   exception ForNext
   exception NoLine
   exception Direct
   exception Mode
   exception Interrupt
   exception Quit
   exception Input
   exception Runtime of string * int option
end

fun prErr s = (TextIO.output (TextIO.stdErr, s ^ "\n"); TextIO.flushOut TextIO.stdErr)
