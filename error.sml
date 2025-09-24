structure Basic =
struct
   exception Syntax of string
   exception NoImpl
   exception Bug of string
   exception RetGosub
   exception NoLine
   exception Direct
   exception Interrupt
   exception Quit
   exception Input of string
end

