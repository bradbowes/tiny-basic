structure Repl : sig
   val loop : (int option * Ast.node) list list *
    (string * int * int * (int option * Ast.node) list) list *
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
         | _            => raise (BasicExn.Bug "Can't evaluate non-expression")

      fun compare a = case a of
           EQ (x, y)    => (eval x) = (eval y)
         | NE (x, y)    => (eval x) <> (eval y)
         | GT (x, y)    => (eval x) > (eval y)
         | GE (x, y)    => (eval x) >= (eval y)
         | LT (x, y)    => (eval x) < (eval y)
         | LE (x, y)    => (eval x) <= (eval y)
         | _            => raise (BasicExn.Bug "Can't compare")

      fun pr ls =
      let
         fun output x = case x of
              STRING s  => s
            | _         =>
                  let val n = eval x
                  in (if n < 0 then "-" else " ") ^ (Int.toString (Int.abs n)) end

         fun sp (s, n) =
            if n <= 0 then s
            else sp (s ^ " ", n - 1)

         fun pad s =
         let
            val sz = String.size s
            val n = 12 - (Int.rem (sz, 12))
         in sp (s, n) end

         fun prItems (ls, s) = case ls of
              []              => "\n"
            | ITEM (i, j)::[] => s ^ (output i) ^ (if j then "" else "\n")
            | ITEM (i, j)::xs => prItems (xs,
                                          let val s = s ^ output i
                                          in if j then s else pad s end)
            | _               => raise (BasicExn.Bug "expected print item")

      in
         print (prItems (ls, ""))
      end

      fun list out =
      let
         fun outputLine (line, stm) =
            TextIO.output (out, Int.toString line ^ " " ^ toString stm ^ "\r\n")
      in app outputLine p end

      fun input (prompt, vars) =
      let
         val msg =
         let val len = length vars
         in
            "INPUT ERROR: expected " ^ Int.toString len ^
            (if len > 1 then " comma separated" else "") ^
            " number" ^ (if len > 1 then "s" else "") ^
            ". Try again..."
         end

         fun try () =
         let
            val line =
            let
               val l = (
                  print (getOpt (prompt, "? "));
                  TextIO.inputLine TextIO.stdIn
               )
            in
               case l of
                    SOME s => s
                  | NONE   => (print "\n"; raise BasicExn.Quit)
            end

            val vals = Parser.parseInput line

            fun insert (env, vars, vals) = case (vars, vals) of
                 ([], [])        => env
               | ([], _)         => raise BasicExn.Input
               | (_::_, [])      => raise BasicExn.Input
               | (x::xs, v::vs)  => insert (StrMap.insert (env, x, v), xs, vs)

         in insert (e, vars, vals) end
      in
         try ()
         handle BasicExn.Input => (prErr msg; input (prompt, vars))
      end

      fun load file =
      let
         val input = TextIO.openIn file

         fun loop prog =
         let val line = TextIO.inputLine  input
         in
            case line of
                 SOME s =>
                     let val stm = Parser.parse s
                     in case stm of
                          LINE x => loop (Prog.insert (prog, x))
                        | NUL    => loop prog
                        | _      => (
                              TextIO.closeIn input;
                              raise BasicExn.Direct )
                     end
               | NONE   => (TextIO.closeIn input; prog)
         end

      in loop [] end

      fun execFor (var, init, limit, inc) =
      let
         fun skipNext (ls, level) = case ls of
              []              => raise BasicExn.ForNext
            | (_, FOR _)::xs  => skipNext (xs, level + 1)
            | (_, NEXT x)::xs => if level = 0 then
                                    if var = getOpt(x, var) then xs
                                    else raise BasicExn.ForNext
                                 else skipNext (xs, level - 1)
            | _::xs           => skipNext (xs, level)

         val init' = eval init
         val limit' = eval limit
         val inc' = case inc of SOME x => eval x | NONE => 1
         val enterLoop = (if inc' < 0 then op< else op>) (limit', init')

         val c' = if enterLoop then tl c else skipNext (tl c, 0)
         val e' = StrMap.insert (e, var, init')
         val fs' = if enterLoop then (var, limit', inc', tl c)::fs else fs
      in (c', gs, fs', p, e') end

      fun execNext opt =
      let
         val (v, limit, inc, cont) = case fs of
              x::xs  => x
            | []     => raise BasicExn.NextFor
         val v' = getOpt (opt, v)
      in
         if v = v' then
            let
               val n = getOpt (StrMap.lookup (e, v), limit) + inc
               val more = (if inc < 0 then op>= else op<=) (n, limit)
               val c' = if more then cont else tl c
               val fs' = if more then fs else tl fs
               val e' = StrMap.insert (e, v, n)
            in (c', gs, fs', p, e') end
         else raise BasicExn.NextFor
      end

      fun applyCompound (line, ls) =
      let
         fun loop (c, ls) = case ls of
              nil    => c
            | x::xs  => loop ((line, x)::c, xs)
      in loop (tl c, ls) end

      fun exec (line, cmd) = (
         case cmd of
              PRINT ls     => pr ls
            | LIST         => list TextIO.stdOut
            | SAVE file    => let val out = TextIO.openOut file
                              in list out; TextIO.closeOut out end
            | BYE          => raise BasicExn.Quit
            | _            => ()
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
            | RUN             => (Prog.getCode p, [], [], p, StrMap.empty)
            | INPUT (pr, ls)  => (tl c, gs, fs, p, input (pr, ls))
            | GOTO n          => (Prog.getContinuation (p, n), gs, fs, p, e)
            | GOSUB n         => (Prog.getContinuation (p, n), (tl c)::gs, fs, p, e)
            | RETURN          => (if null gs then raise BasicExn.RetGosub else hd gs,
                                    tl gs, fs, p, e)
            | COMP ls         => (applyCompound (line, ls), gs, fs, p, e)
            | END             => ([], [], [], p, e)
            | FOR x           => execFor x
            | NEXT x          => execNext x
            | _               => (tl c, gs, fs, p, e)
      ) handle
           BasicExn.RetGosub     => raise (BasicExn.Runtime ("RETURN without GOSUB", line))
         | BasicExn.NextFor      => raise (BasicExn.Runtime ("NEXT without FOR", line))
         | BasicExn.ForNext      => raise (BasicExn.Runtime ("FOR without NEXT", line))
         | BasicExn.NoLine       => raise (BasicExn.Runtime ("Undefined line number", line))
         | BasicExn.Bug msg      => raise (BasicExn.Runtime ("Interpreter bug: " ^ msg, line))
         | BasicExn.Direct       => raise (BasicExn.Runtime (
                                    "Direct command in program text", line))
         | BasicExn.Syntax msg   => raise (BasicExn.Syntax msg)
         | BasicExn.Quit         => raise BasicExn.Quit
         | x                     => raise (BasicExn.Runtime (exnMessage x, line))

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
         interp ([(NONE, Parser.parse input)], gs, fs, p, e)
         handle
              BasicExn.Syntax msg   => (prErr ("SYNTAX ERROR: " ^ msg); (gs, fs, p, e))
            | BasicExn.Runtime (msg, ln)  => (
                  prErr ("RUNTIME ERROR: " ^ msg ^
                        (case ln of SOME n => " in line " ^ (Int.toString n) | NONE => ""));
                  (gs, fs, p, e) )

   in case line of
        SOME s => (loop (exec s) handle BasicExn.Quit => ())
      | NONE   => (print "\n"; ())
   end

end
