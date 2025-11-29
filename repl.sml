structure Repl : sig
   val init : Ast.node list -> unit
end  =
struct
   open Ast
   fun interp (c, gs, fs, p, e) = let
      fun eval a = case a of
           NUM n        => n
         | VAR s        => getOpt (StrMap.lookup (e, s), 0)
         | NEG n        => ~ (eval n)
         | ADD (x, y)   => (eval x) + (eval y)
         | SUB (x, y)   => (eval x) - (eval y)
         | MUL (x, y)   => (eval x) * (eval y)
         | DIV (x, y)   => (eval x) div (eval y)
         | _            => raise (BasicExn.Bug "Can't evaluate non-expression")

      fun compare a = case a of
           EQ (x, y)    => (eval x) = (eval y)
         | NE (x, y)    => (eval x) <> (eval y)
         | GT (x, y)    => (eval x) > (eval y)
         | GE (x, y)    => (eval x) >= (eval y)
         | LT (x, y)    => (eval x) < (eval y)
         | LE (x, y)    => (eval x) <= (eval y)
         | _            => raise (BasicExn.Bug "Can't compare")

      fun pr ls = let
         fun output x = case x of
              STRING s  => s
            | _         =>
                  let val n = eval x
                  in (if n < 0 then "-" else " ") ^ (Int.toString (Int.abs n)) end

         fun sp (s, n) =
            if n <= 0 then s
            else sp (s ^ " ", n - 1)

         fun pad s = let
            val sz = String.size s
            val n = 12 - (Int.rem (sz, 12))
         in sp (s, n) end

         fun prItems (ls, s) = case ls of
              []              => "\n"
            | ITEM (i, j) :: []  => s ^ (output i) ^ (if j then "" else "\n")
            | ITEM (i, j) :: xs  => prItems (xs,
                                          let val s = s ^ output i
                                          in if j then s else pad s end)
            | _               => raise (BasicExn.Bug "expected print item")

      in print (prItems (ls, "")) end

      fun list (start, finish, out) = let
         val test = case (start, finish) of
              (NONE, NONE)       => (fn (l, _) => true)
            | (SOME m, NONE)     => (fn (l, _) => l >= m)
            | (SOME m, SOME n)   => (fn (l, _) => l >= m andalso l <= n)
            | (NONE, SOME n)     => (fn (l, _) => l <= n)

         fun outputLine (line, stm) =
            TextIO.output (out, Int.toString line ^ " " ^ toString stm ^ "\r\n")

      in app outputLine (List.filter test p) end

      fun input (prompt, vars) = let
         val msg = let
            val len = length vars
         in
            "INPUT ERROR: expected " ^ Int.toString len ^
            (if len > 1 then " comma separated" else "") ^
            " number" ^ (if len > 1 then "s" else "") ^
            ". Try again..."
         end

         fun try () = let
            val line = let
               val l = (
                  print (getOpt (prompt, "? "));
                  TextIO.inputLine TextIO.stdIn )
            in case l of
                 SOME s => s
               | NONE   => (print "\n"; raise BasicExn.Quit)
            end

            val vals = Parser.parseInput line

            fun insert (env, vars, vals) = case (vars, vals) of
                 ([], [])           => env
               | ([], _)            => raise BasicExn.Input
               | (_ :: _, [])       => raise BasicExn.Input
               | (x :: xs, v :: vs) => insert (StrMap.insert (env, x, v), xs, vs)

         in insert (e, vars, vals) end
      in
         try ()
         handle BasicExn.Input => (prErr msg; input (prompt, vars))
      end

      fun load file = let
         val input = TextIO.openIn file

         fun loop prog = let
            val line = TextIO.inputLine input
         in case line of
              SOME s => let
                     val stm = Parser.parse s
                  in
                     case stm of
                          LINE x => loop (Prog.insert (prog, x))
                        | NUL    => loop prog
                        | _      => (
                              TextIO.closeIn input;
                              raise (BasicExn.Runtime "Direct command in program text") )
                  end
            | NONE   => (TextIO.closeIn input; prog)
         end

      in loop [] end

      fun execFor (var, init, limit, inc) = let
         fun skipNext (ls, level) = case ls of
              []                 => raise (BasicExn.Runtime "FOR without NEXT")
            | (_, FOR _) :: xs   => skipNext (xs, level + 1)
            | (_, NEXT x) :: xs  => if level = 0 then
                                       if var = getOpt(x, var) then xs
                                       else raise (BasicExn.Runtime "FOR without NEXT")
                                    else skipNext (xs, level - 1)
            | _ :: xs            => skipNext (xs, level)

         val init' = eval init
         val limit' = eval limit
         val inc' = case inc of SOME x => eval x | NONE => 1
         val enterLoop = (if inc' < 0 then op < else op >) (limit', init')
         val c' = if enterLoop then tl c else skipNext (tl c, 0)
         val e' = StrMap.insert (e, var, init')
         val fs' = if enterLoop then (var, limit', inc', tl c) :: fs else fs
      in (c', gs, fs', p, e') end

      fun execNext opt = let
         val exn = BasicExn.Runtime "NEXT without FOR"
         val (v, limit, inc, cont) = case fs of
              x :: xs   => x
            | []        => raise exn
         val v' = getOpt (opt, v)
      in
         if v = v' then
            let
               val n = getOpt (StrMap.lookup (e, v), limit) + inc
               val more = (if inc < 0 then op >= else op <=) (n, limit)
               val c' = if more then cont else tl c
               val fs' = if more then fs else tl fs
               val e' = StrMap.insert (e, v, n)
            in (c', gs, fs', p, e') end
         else raise exn
      end

      fun run f = let
         val p' = case f of
              SOME s => load s
            | NONE   => p
      in (Prog.getCode p', [], [], p', StrMap.empty) end

      fun exec (line, cmd) = let
         fun errMsg msg =
            prErr ("ERROR: " ^ msg ^
                   (case line of SOME n => " in line " ^ (Int.toString n) | NONE => ""))

         val modeErr = "Interactive command in run mode"
      in (
         case line of
              SOME _ => (case cmd of
                             NEW       => raise (BasicExn.Runtime modeErr)
                           | LOAD _    => raise (BasicExn.Runtime modeErr)
                           | RENUM _   => raise (BasicExn.Runtime modeErr)
                           | SAVE _    => raise (BasicExn.Runtime modeErr)
                           | LIST _    => raise (BasicExn.Runtime modeErr)
                           | _         => () )
            | NONE => ()
            ;

         case cmd of
              IF (x, y)       => if compare x then exec (line, y) else (tl c, gs, fs, p, e)
            | LINE ln         => (tl c, gs, fs, Prog.insert (p, ln), e)
            | DEL ln          => (tl c, gs, fs, Prog.delete (p, ln), e)
            | NEW             => ([], [], [], [], StrMap.empty)
            | LOAD file       => ([], [], [], load file, StrMap.empty)
            | RENUM (x, y)    => ([], [], [], Prog.renum (p, x, y), e)
            | LET (x, y)      => (tl c, gs, fs, p, StrMap.insert (e, x, eval y))
            | CLEAR           => (tl c, [], [], p, StrMap.empty)
            | RUN f           => run f
            | INPUT (pr, ls)  => (tl c, gs, fs, p, input (pr, ls))
            | GOTO n          => (Prog.getContinuation (p, n), gs, fs, p, e)
            | GOSUB n         => (Prog.getContinuation (p, n), (tl c) :: gs, fs, p, e)
            | RETURN          => (if null gs
                                  then raise (BasicExn.Runtime "RETURN without GOSUB")
                                  else hd gs,
                                    tl gs, fs, p, e)
            | COMP ls         => (map (fn x => (line, x)) ls @ tl c, gs, fs, p, e)
            | END             => ([], [], [], p, e)
            | FOR x           => execFor x
            | NEXT x          => execNext x
            | PRINT ls        => (pr ls; (tl c, gs, fs, p, e))
            | LIST (x, y)     => (list (x, y, TextIO.stdOut); (tl c, gs, fs, p, e))
            | SAVE file       => let val out = TextIO.openOut file
                                 in
                                    list (NONE, NONE, out);
                                    TextIO.closeOut out;
                                    (tl c, gs, fs, p, e)
                                 end
            | BYE             => raise BasicExn.Quit
            | ERR (_, msg)    => raise (BasicExn.Runtime msg)
            | _               => (tl c, gs, fs, p, e)
         )
         handle
              BasicExn.Runtime msg  => (errMsg msg; ([], gs, fs, p, e))
            | BasicExn.Quit         => raise BasicExn.Quit
            | ex                    => (errMsg (exnMessage ex); ([], gs, fs, p, e))
      end

   in case c of
        []        => (gs, fs, p, e)
      | x :: xs   => interp (exec x)
   end

   fun loop (gs, fs, p, e) = let
      val line = (
         print "*> ";
         TextIO.inputLine TextIO.stdIn
      )

      fun exec input = interp ([(NONE, Parser.parse input)], gs, fs, p, e)
      handle BasicExn.Syntax msg => (prErr ("SYNTAX ERROR: " ^ msg); (gs, fs, p, e) )

   in case line of
        SOME s => (loop (exec s) handle BasicExn.Quit => ())
      | NONE   => (print "\n"; ())
   end

   fun init stm = let
      val ls = map (fn x => (NONE, x)) stm
      fun exec () = interp (ls, [], [], [], StrMap.empty)
   in
      loop (exec ()) handle BasicExn.Quit => ()
   end

end
