structure Repl : sig
   val loop : Ast.node list list * (int * Ast.node) list * int StrMap.map -> unit
end  =
struct
   open Ast
   fun interp (c, s, p, e) =
   let
      fun eval a =
         case a of
              NUM n        => n
            | VAR s        =>
               let
                  val v = StrMap.lookup (e, s)
               in
                  case v of
                       SOME n => n
                     | NONE   => 0
               end
            | NEG n        => ~ (eval n)
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
                 STRING s  => s
               | _         =>
                     let val n = eval i
                     in
                        (if n < 0 then "-" else "") ^
                        (Int.toString (Int.abs n))
                     end
      in
         print (String.concatWith " " (map output ls) ^ "\n")
      end

      fun list out =
      let
         fun outputLine (line, stm) =
            TextIO.output (out, Int.toString line ^ " " ^ toString stm ^ "\r\n")
      in
         app outputLine p
      end

      fun input vars =
      let
         val msg = let
            val len = length vars
         in
            "expected " ^ Int.toString len ^
            (if len > 1 then " comma separated" else "") ^
            " number" ^ (if len > 1 then "s" else "") ^
            ". Try again..."
         end

         fun try () =
         let
            val line =
            let
               val l = (
                  print "? ";
                  TextIO.inputLine TextIO.stdIn
               )
            in
               case l of
                    SOME s => s
                  | NONE   => ""
            end

            val vals = Parser.parseInput line

            fun insert (env, vars, vals) =
               case (vars, vals) of
                    ([], [])        => env
                  | ([], _)         => raise Basic.Input
                  | (_::_, [])      => raise Basic.Input
                  | (x::xs, v::vs)  => insert (StrMap.insert (env, toString x, v), xs, vs)

         in
            insert (e, vars, vals)
         end
      in
         try ()
         handle
              Basic.Input     => (
                  print ("INPUT ERROR: " ^ msg ^ "\n");
                  input vars )
            | Basic.Syntax _  => (
                  print ("INPUT ERROR: " ^ msg ^ "\n");
                  input vars )
      end

      fun load file =
      let
         val input = TextIO.openIn file

         fun loop prog =
         let
            val line = TextIO.inputLine  input
         in
            case line of
                 SOME s =>
                     let val stm = Parser.parse s
                     in
                        case stm of
                             LINE x => loop (Prog.insert (prog, x))
                           | _      => (
                                 TextIO.closeIn input;
                                 raise Basic.Direct )
                     end
               | NONE   => (TextIO.closeIn input; prog)
         end
      in
         loop []
      end

      fun exec cmd =
      let
         val p' = case cmd of
              LINE ln      => Prog.insert (p, ln)
            | DEL ln       => Prog.delete (p, ln)
            | NEW          => []
            | LOAD file    => load file
            | _            => p

         val e' = case cmd of
              LET (x, y)   => StrMap.insert (e, toString x, eval y)
            | CLEAR        => StrMap.empty
            | RUN          => StrMap.empty
            | NEW          => StrMap.empty
            | LOAD _       => StrMap.empty
            | INPUT ls     => input ls
            | _            => e

         val c' = case cmd of
              GOTO x       => Prog.goto (p, eval x)
            | GOSUB x      => Prog.goto (p, eval x)
            | RETURN       => if null s then raise Basic.RetGosub
                              else hd s
            | RUN          => map #2 p
            | END          => []
            | NEW          => []
            | LOAD _       => []
            | _            => tl c

         val s' = case cmd of
              GOSUB _      => (tl c)::s
            | RETURN       => tl s
            | NEW          => []
            | LOAD _       => []
            | END          => []
            | _            => s

      in
         case cmd of
              PRINT ls     => pr ls
            | LIST         => list TextIO.stdOut
            | SAVE file    => let val out = TextIO.openOut file
                              in list out; TextIO.closeOut out end
            | BYE          => raise Basic.Quit
            | _            => ()
            ;

         case cmd of
              IF (x, y)    => if compare x then exec y else (c', s', p', e')
            | _            => (c', s', p', e')

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
            | Basic.Input        => (print "INPUT ERROR\n"; (s, p, e))
            | Basic.NoImpl       => (print "FEATURE NOT IMPLEMENTED\n"; (s, p, e))
            | Basic.RetGosub     => (print "ERROR: RETURN without GOSUB\n"; (s, p, e))
            | Basic.NoLine       => (print "ERROR: Line number undefined\n"; (s, p, e))
            | Basic.Bug msg      => (print ("COMPILER ERROR: " ^ msg ^ "\n"); (s, p, e))
            | Basic.Direct       => (print "ERROR: Command in program text\n"; (s, p, e))
            | Fail msg           => (print ("ERROR: " ^ msg ^ "\n"); (s, p, e))
            | Basic.Quit         => raise Basic.Quit
            | x                  => (print ("ERROR: " ^ (exnMessage x) ^ "\n"); (s, p, e))
   in
      case line of
           SOME s => (loop (exec s) handle x => ())
         | NONE   => ()
   end

end
