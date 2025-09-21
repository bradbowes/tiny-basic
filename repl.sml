structure Repl : sig
   val interp : Ast.node list * Ast.node list list * (int * Ast.node) list * int StrMap.map
                -> Ast.node list list * (int * Ast.node) list * int StrMap.map
   val loop : Ast.node list list * (int * Ast.node) list * int StrMap.map -> unit
end  =
struct
   open Ast
   fun interp (c, s, p, e) =
      let
         fun eval a =
            case a of
                 NUM (n)      => n
               | VAR (s)      => let
                                    val v = StrMap.lookup (e, s)
                                 in
                                    case v of
                                         SOME (n)  => n
                                       | NONE      => 0
                                 end
               | NEG (n)      => ~(eval n)
               | ADD (x, y)   => (eval x) + (eval y)
               | SUB (x, y)   => (eval x) - (eval y)
               | MUL (x, y)   => (eval x) * (eval y)
               | DIV (x, y)   => (eval x) div (eval y)
               | _            => raise (Basic.Bug "Can't evaluate non-expression")

         fun compare a =
            case a of
                 EQ (x, y)    => (eval x) = (eval y)
               | NE (x, y)    => (eval x) <> (eval y)
               | GT (x, y)    => (eval x) > (eval y)
               | GE (x, y)    => (eval x) >= (eval y)
               | LT (x, y)    => (eval x) < (eval y)
               | LE (x, y)    => (eval x) <= (eval y)
               | _            => raise (Basic.Bug "Can't compare")

         fun pr ls =
            let
               fun output i =
                  case i of
                       STRING (s)   => s
                     | _            => let val n = eval i
                                       in
                                          (if n < 0 then "-" else "") ^
                                          (Int.toString (Int.abs n))
                                       end
            in
               print (String.concatWith " " (map output ls) ^ "\n")
            end

         fun list () =
            let
               fun ls (line, stm) =
                  print (Int.toString line ^ " " ^ toString stm ^ "\n")
            in
               app ls p
            end

         fun input ls =
            let
               val line = (
                  print "? ";
                  TextIO.inputLine TextIO.stdIn
               )
            in
               raise Basic.NoImpl
            end

         fun exec cmd =
            let
               val prog = case cmd of
                    LINE ln      => Prog.insert (p, ln)
                  | DEL ln       => Prog.delete (p, ln)
                  | _            => p

               val env = case cmd of
                    LET (x, y)   => StrMap.insert (e, toString x, eval y)
                  | CLEAR        => StrMap.empty
                  | RUN          => StrMap.empty
                  | _            => e

               val cont = case cmd of
                    GOTO x       => Prog.goto (p, eval x)
                  | GOSUB x      => Prog.goto (p, eval x)
                  | RETURN       => if null s then raise Basic.RetGosub
                                    else hd s
                  | RUN          => map #2 p
                  | END          => []
                  | _            => tl c

               val stack = case cmd of
                    GOSUB _      => (tl c)::s
                  | RETURN       => tl s
                  | _            => s

            in
               case cmd of
                    PRINT (ls)   => pr ls
                  | LIST         => list ()
                  | INPUT x      => raise Basic.NoImpl
                  | _            => ()
                  ;

               case cmd of
                    IF (x, y)    => if compare x then exec y else (cont, stack, prog, env)
                  | _            => (cont, stack, prog, env)

            end

      in
         case c of
              []     => (s, p, e)
            | x::xs  => interp (exec x)

      end

   fun loop (s, p, e) =
      let
         val line = (
            print "*> ";
            TextIO.inputLine TextIO.stdIn
         )

         fun exec input =
            interp ([Parser.parse input], s, p, e)
            handle
                 Basic.Syntax msg   => (print ("SYNTAX ERROR: " ^ msg ^ "\n"); (s, p, e))
               | Basic.NoImpl       => (print "FEATURE NOT IMPLEMENTED\n"; (s, p, e))
               | Basic.RetGosub     => (print "ERROR: RETURN without GOSUB\n"; (s, p, e))
               | Basic.NoLine       => (print "ERROR: Line number undefined\n"; (s, p, e))
               | Basic.Bug msg      => (print ("COMPILER ERROR: " ^ msg ^ "\n"); (s, p, e))
               | Fail msg           => (print ("ERROR: " ^ msg ^ "\n"); (s, p, e))
               | x                  => (print ("ERROR: " ^ (exnMessage x) ^ "\n"); (s, p, e))
      in
         case line of
              SOME s => loop (exec s)
            | NONE   => ()
      end

end

