structure Repl : sig
   val loop : Ast.node list list *
    (string * int * int * Ast.node list) list *
   (int * Ast.node) list * int StrMap.map -> unit
end  =
struct
   open Ast
   fun interp (c, gs, fs, p, e) =
   let
      fun eval a = case a of
           NUM n        => n
         | VAR s        => getOpt (StrMap.lookup (e, s), 0)
         | NEG n        => ~ (eval n)
         | ADD (x, y)   => (eval x) + (eval y)
         | SUB (x, y)   => (eval x) - (eval y)
         | MUL (x, y)   => (eval x) * (eval y)
         | DIV (x, y)   => (eval x) div (eval y)
         | _            => raise (Basic.Bug "Can't evaluate non-expression")

      fun compare a = case a of
           EQ (x, y)    => (eval x) = (eval y)
         | NE (x, y)    => (eval x) <> (eval y)
         | GT (x, y)    => (eval x) > (eval y)
         | GE (x, y)    => (eval x) >= (eval y)
         | LT (x, y)    => (eval x) < (eval y)
         | LE (x, y)    => (eval x) <= (eval y)
         | _            => raise (Basic.Bug "Can't compare")

      fun pr ls =
      let
         fun output x = case x of
              STRING s  => s
            | _         =>
                  let val n = eval x
                  in (if n < 0 then "-" else " ") ^ (Int.toString (Int.abs n)) end

         fun prItems (ls, s) = case ls of
              []              => s ^ "\n"
            | ITEM (i, j)::[] => s ^ (output i) ^ (if j then "" else "\n")
            | ITEM (i, j)::xs => prItems (
                                    xs,
                                    s ^ (output i) ^ (if j then "" else " "))
            | _               => raise (Basic.Bug "expected print item")

      in
         print (prItems (ls, ""))
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
         val msg =
         let
            val len = length vars
         in
            "INPUT ERROR: expected " ^ Int.toString len ^
            (if len > 1 then " comma separated" else "") ^
            " number" ^ (if len > 1 then "s" else "") ^
            ". Try again...\n"
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
               getOpt (l, "")
            end

            val vals = Parser.parseInput line

            fun insert (env, vars, vals) = case (vars, vals) of
                 ([], [])        => env
               | ([], _)         => raise Basic.Input
               | (_::_, [])      => raise Basic.Input
               | (x::xs, v::vs)  => insert (StrMap.insert (env, x, v), xs, vs)

         in
            insert (e, vars, vals)
         end
      in
         try ()
         handle
              Basic.Input     => (print msg; input vars)
            | Basic.Syntax _  => (print msg; input vars)
      end

      fun load file =
      let
         val input = TextIO.openIn file

         fun loop prog =
         let
            val line = TextIO.inputLine  input
         in case line of
              SOME s =>
                  let val stm = Parser.parse s
                  in case stm of
                       LINE x => loop (Prog.insert (prog, x))
                     | NUL    => loop prog
                     | _      => (
                           TextIO.closeIn input;
                           raise Basic.Direct )
                  end
            | NONE   => (TextIO.closeIn input; prog)
         end

      in
         loop []
      end

      fun nextIncrement opt =
      let
         val (v, limit, inc, cont) = case fs of
              x::xs  => x
            | []     => raise Basic.NextFor
         val v' = getOpt (opt, v)
      in
         if v = v' then
            let
               val n = getOpt (StrMap.lookup (e, v), 0) + inc
               val c' = if n <= limit then cont else tl c
               val fs' = if n <= limit then fs else tl fs
               val e' = StrMap.insert (e, v, n)
            in
               (c', gs, fs', p, e')
            end
         else raise Basic.NextFor
      end

      fun exec cmd =
      let
         val p' = case cmd of
              LINE ln      => Prog.insert (p, ln)
            | DEL ln       => Prog.delete (p, ln)
            | NEW          => []
            | LOAD file    => load file
            | RENUM (x, y) => Prog.renum (p, x, y)
            | _            => p

         val e' = case cmd of
              LET (x, y)   => StrMap.insert (e, x, eval y)
            | CLEAR        => StrMap.empty
            | RUN          => StrMap.empty
            | NEW          => StrMap.empty
            | LOAD _       => StrMap.empty
            | INPUT ls     => input ls
            | FOR
              (v, i, _, _) => StrMap.insert (e, v, eval i)
            | _            => e

         val c' = case cmd of
              GOTO n       => Prog.goto (p, n)
            | GOSUB n      => Prog.goto (p, n)
            | RETURN       => if null gs then raise Basic.RetGosub
                              else hd gs
            | COMP ls      => let fun loop (c, ls) =
                                 case ls of [] => c | x::xs => loop (x::c, xs)
                              in loop (tl c, ls) end
            | RUN          => map #2 p
            | END          => []
            | NEW          => []
            | LOAD _       => []
            | _            => tl c

         val gs' = case cmd of
              GOSUB _      => (tl c)::gs
            | RETURN       => tl gs
            | NEW          => []
            | LOAD _       => []
            | END          => []
            | _            => gs

         val fs' = case cmd of
              FOR (v, _, limit, inc)   => (v, eval limit,
                                          eval (getOpt (inc, NUM 1)),
                                          tl c)::fs
            | _                        => fs

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
              IF (x, y)    => if compare x then exec y else (c', gs', fs', p', e')
            | NEXT x       => nextIncrement x
            | _            => (c', gs', fs', p', e')

      end

   in case c of
        []     => (gs, fs, p, e)
      | x::xs  => interp (exec x)

   end

   fun loop (gs, fs, p, e) =
   let
      val line = (
         print "*> ";
         TextIO.inputLine TextIO.stdIn
      )

      fun exec input =
         interp ([Parser.parse input], gs, fs, p, e)
         handle
              Basic.Syntax msg   => (print ("SYNTAX ERROR: " ^ msg ^ "\n"); (gs, fs, p, e))
            | Basic.Input        => (print "INPUT ERROR\n"; (gs, fs, p, e))
            | Basic.NoImpl       => (print "FEATURE NOT IMPLEMENTED\n"; (gs, fs, p, e))
            | Basic.RetGosub     => (print "ERROR: RETURN without GOSUB\n"; (gs, fs, p, e))
            | Basic.NextFor      => (print "ERROR: NEXT without FOR\n"; (gs, fs, p, e))
            | Basic.NoLine       => (print "ERROR: Line number undefined\n"; (gs, fs, p, e))
            | Basic.Bug msg      => (print ("COMPILER ERROR: " ^ msg ^ "\n"); (gs, fs, p, e))
            | Basic.Direct       => (print "ERROR: Command in program text\n"; (gs, fs, p, e))
            | Fail msg           => (print ("ERROR: " ^ msg ^ "\n"); (gs, fs, p, e))
            | Basic.Quit         => raise Basic.Quit
            | x                  => (print ("ERROR: " ^ (exnMessage x) ^ "\n"); (gs, fs, p, e))

   in case line of
        SOME s => (loop (exec s) handle x => ())
      | NONE   => (print "\n"; ())
   end

end
