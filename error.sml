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

