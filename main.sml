structure Main =
struct

   fun basic args =
   let
      val x = case args of
           []     => (print "MY TINY BASIC\n"; [])
         | x::_   => [Ast.RUN (SOME x)]
   in Repl.init x end

   fun main _ = (
      basic (tl (CommandLine.arguments ()));
      0
   )

   fun mltonMain () = basic (CommandLine.arguments ())

end
