structure Test =
struct

   fun test (prog, env) =
      let
         val line = (
               print "> ";
               TextIO.inputLine TextIO.stdIn
            )

         fun exec s =
            let
               val (n, stm) = Parser.parse s
            in
               case n of
                    SOME (n)  => (
                           case stm of
                                Ast.NUL   => (Prog.delete (prog, n), env)
                              | _         => (Prog.insert (prog, (n, stm)), env)
                        )
                  | NONE      => (prog, Ast.exec (stm, prog, env))
            end
            handle
                 Basic.Syntax s  => (print ("SYNTAX ERROR: " ^ s ^ "\n"); (prog, env))
               | Basic.NoImpl    => (print "FEATURE NOT IMPLEMENTED\n"; (prog, env))
               | Basic.Bug s     => (print ("COMPILER ERROR: " ^ s ^ "\n"); (prog, env))
               | Fail s          => (print ("ERROR: " ^ s ^ "\n"); (prog, env))
               | _               => (print "ERROR: Unknown error.\n"; (prog, env))
      in
         case line of
              SOME s => test (exec s)
            | NONE   => ()
      end

   fun main _ = (
      print "MY TINY BASIC\n\n";
      test ([], StrMap.empty);
      0
   )

end
