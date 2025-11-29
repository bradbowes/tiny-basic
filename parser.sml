structure Parser =
struct
   fun parse line = let
      val scan = Scanner.scanner line

      fun eat (tok, c, err) = let
         val (t, c') = scan c
      in
         if t = tok then c'
         else raise (BasicExn.Syntax err)
      end

      fun getAtom (t, c) = case t of
           Token.NUM n  => ((Ast.NUM n), c)
         | Token.VAR v  => ((Ast.VAR v), c)
         | Token.LPAREN => let
               val (e, c) = getExpression (scan c)
               val c = eat (Token.RPAREN, c, "missing \")\"")
            in (e, c) end
         | _            => raise (BasicExn.Syntax "missing numeric expression")

      and getUnary (t, c) = case t of
           Token.MINUS  => let val (a, c) = getAtom (scan c)
                           in (Ast.NEG a, c) end
         | _            => getAtom (t, c)

      and getProduct tk = let
         fun loop (left, cont) = let
            val (opr, c) = (scan cont)

            fun build f = let
               val (right, c) = getUnary (scan c)
            in loop (f (left, right), c) end

         in case opr of
              Token.MUL => build Ast.MUL
            | Token.DIV => build Ast.DIV
            | _         => (left, cont)
         end
      in loop (getUnary tk) end

      and getExpression tk = let
         fun loop (left, cont) = let
            val (opr, c) = (scan cont)

            fun build f = let
               val (right, c) = getProduct (scan c)
            in loop (f (left, right), c) end

         in case opr of
              Token.PLUS   => build Ast.ADD
            | Token.MINUS  => build Ast.SUB
            | _            => (left, cont)
         end
      in loop (getProduct tk) end

      fun getPrintStm c = let
         fun getItem (t, c) = case t of
              Token.STRING s  => SOME (Ast.STRING s, c)
            | Token.MINUS     => SOME (getExpression (t, c))
            | Token.LPAREN    => SOME (getExpression (t, c))
            | Token.VAR _     => SOME (getExpression (t, c))
            | Token.NUM _     => SOME (getExpression (t, c))
            | _               => NONE

         fun loop (ls, c) = let
            val item = getItem (scan c)
         in case item of
              SOME (x, c)  => let
                  val (sep, c') = scan c
               in case sep of
                    Token.SEMICOLON => loop (Ast.ITEM (x, true) :: ls, c')
                  | Token.COMMA     => loop (Ast.ITEM (x, false) :: ls, c')
                  | _               => (Ast.PRINT (List.rev (Ast.ITEM (x, false) :: ls)), c)
               end
            | NONE         => (Ast.PRINT (List.rev ls), c)
         end
      in loop ([], c) end

      fun getCompare tk = let
         val (left, c) = getExpression tk
         val (opr, c) = (scan c)
         val node = case opr of
              Token.EQ => Ast.EQ
            | Token.NE => Ast.NE
            | Token.GT => Ast.GT
            | Token.GE => Ast.GE
            | Token.LT => Ast.LT
            | Token.LE => Ast.LE
            | _        => raise (BasicExn.Syntax "missing comparison")
         val (right, c) = getExpression (scan c)
      in (node (left, right), c) end

      fun getGoStm f (t, c) = case t of
           Token.NUM n  => ((f n), c)
         | _            => raise (BasicExn.Syntax "missing line number")

      fun getFileCmd f (file, c) = case file of
           Token.STRING s  => ((f s), c)
         | _               => raise (BasicExn.Syntax "missing file name string")

      fun getVar t = case t of
           Token.VAR v  => v
         | _            => raise (BasicExn.Syntax "missing variable")

      fun getLetStm (t, c) = let
         val v = getVar t
         val c = eat (Token.EQ, c, v ^ " is not a command")
         val (e, c) = getExpression (scan c)
      in (Ast.LET (v, e), c) end

      fun getForStm (t, c) = let
         val v = getVar t
         val c = eat (Token.EQ, c, "missing \"=\"")
         val (init, c) = getExpression (scan c)
         val c = eat (Token.TO, c, "missing TO")
         val (limit, c) = getExpression (scan c)
         val (t, c') = scan c
      in case t of
           Token.STEP   => let val (inc, c) = getExpression (scan c')
                           in (Ast.FOR (v, init, limit, SOME inc), c) end
         | _            => (Ast.FOR (v, init, limit, NONE), c)
      end

      fun getNextStm (t, c) = let
         val (t, c') = scan c
      in case t of
           Token.VAR v  => (Ast.NEXT (SOME v), c')
         | _            => (Ast.NEXT NONE, c)
      end

      fun getInputStm (t, c) = let
         val (prompt, (t, c)) = case t of
              Token.STRING s  => let val c = eat (Token.COMMA, c, "missing \",\"")
                                 in (SOME s, (scan c)) end
            | _               => (NONE, (t, c))

         fun loop (ls, c) = let
            val (t, c') = (scan c)
         in case t of
              Token.COMMA  => let val (t, c) = scan c'
                              in loop ((getVar t) :: ls, c) end
            | _            => (Ast.INPUT (prompt, (List.rev ls)), c)
         end
      in loop ([getVar t], c) end

      fun getOptPair (cmd, sep, c) = let
         val (t, c') = scan c
         val (opt1, c) = case t of
              Token.NUM n  => (SOME n, c')
            | _            => (NONE, c)
         val (t, c') = scan c
         val (opt2, c) = if t = sep
                         then let val (t, c) = scan c'
                              in case t of Token.NUM n => (SOME n, c) | _ => (NONE, c') end
                         else (NONE, c)
      in (cmd (opt1, opt2), c) end

      fun getRunCmd c = let
         val (t, c') = scan c
      in case t of
           Token.STRING s  => (Ast.RUN (SOME s), c')
         | _               => (Ast.RUN NONE, c)
      end

      fun getStatement (t, c) = case t of
           Token.PRINT     => getPrintStm c
         | Token.EOL       => (Ast.NUL, c)
         | Token.COLON     => (Ast.NUL, c)
         | Token.IF        => getIfStm (scan c)
         | Token.GO        => let val (t, c) = scan c in case t of
                                   Token.TO  => getGoStm Ast.GOTO (scan c)
                                 | Token.SUB => getGoStm Ast.GOSUB (scan c)
                                 | _         => raise (BasicExn.Syntax "missing GOTO or GOSUB")
                              end
         | Token.GOTO      => getGoStm Ast.GOTO (scan c)
         | Token.GOSUB     => getGoStm Ast.GOSUB (scan c)
         | Token.RETURN    => (Ast.RETURN, c)
         | Token.CLEAR     => (Ast.CLEAR, c)
         | Token.NEW       => (Ast.NEW, c)
         | Token.LOAD      => getFileCmd Ast.LOAD (scan c)
         | Token.SAVE      => getFileCmd Ast.SAVE (scan c)
         | Token.END       => (Ast.END, c)
         | Token.LET       => getLetStm (scan c)
         | Token.VAR _     => getLetStm (t, c)
         | Token.INPUT     => getInputStm (scan c)
         | Token.FOR       => getForStm (scan c)
         | Token.NEXT      => getNextStm (t, c)
         | Token.REM s     => (Ast.REM s, c)
         | Token.TICK s    => (Ast.TICK s, c)
         | Token.LIST      => getOptPair (Ast.LIST, Token.MINUS, c)
         | Token.RUN       => getRunCmd c
         | Token.BYE       => (Ast.BYE, c)
         | Token.RENUM     => getOptPair (Ast.RENUM, Token.COMMA, c)
         | _               => raise (BasicExn.Syntax "missing command")

      and getCompoundStm tk = let
         fun loop (ls, tk) = let
            val (s, c) = getStatement tk
            val (t, c') = case s of Ast.NUL => tk | _ => (scan c)
         in case t of
              Token.COLON  => loop (s :: ls, (scan c'))
            | Token.TICK x => (Ast.COMP (List.rev (Ast.TICK x :: s :: ls)), c')
            | Token.EOL    => (case ls of
                                   []        => (s, c)
                                 | x :: xs   => (Ast.COMP (List.rev (s :: ls)), c) )
            | _            => raise (BasicExn.Syntax "expected end of line")
         end
      in loop ([], tk) end

      and getIfStm (t, c) = let
         val (tst, c) = getCompare (t, c)
         val c = eat (Token.THEN, c, "missing THEN")
         val (t, c) = scan c
         val (stm, c) = case t of
              Token.NUM n  => (Ast.GOTO n, c)
            | _            => getCompoundStm (t, c)
      in (Ast.IF (tst, stm), c) end

      fun getLine (t, c) = case t of
           Token.NUM (n) => let
                  val (s, _) = getCompoundStm (scan c)
                  handle
                     BasicExn.Syntax msg => (
                        prErr ("SYNTAX ERROR: " ^ msg ^ " in line " ^ Int.toString n);
                        (Ast.ERR (String.extract (line, c, NONE), msg), size line) )
               in case s of
                    Ast.NUL   => Ast.DEL n
                  | _         => Ast.LINE (n, s)
               end
         | _ => let val (s, _) = getCompoundStm (t, c) in s end

   in getLine (scan 0) end

   fun parseInput line = let
      val scan = Scanner.scanner line

      fun getNumber t = case t of
           Token.NUM n  => n
         | _            => raise (BasicExn.Input)

      fun getValue (t, c) = case t of
           Token.MINUS     => let
                                 val (t, c) = (scan c)
                                 val n = getNumber t
                              in
                                 (~ n, c)
                              end
         | _               => (getNumber t, c)

      fun getValues (ls, tk) = let
         val (n, c) = getValue tk
         val (t, c) = (scan c)
      in case t of
           Token.COMMA  => getValues (n :: ls, scan c)
         | Token.EOL    => List.rev (n :: ls)
         | _            => raise (BasicExn.Input)
      end

   in getValues ([], scan 0) end

end
