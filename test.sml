structure Test =
struct

   fun main _ = (
      print "MY TINY BASIC\n\n";
      Repl.loop ([], [], StrMap.empty);
      0
   )

end
