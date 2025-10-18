structure Parser =
struct
   fun parse line =
   let
      val scan = Scanner.scanner line

      fun getAtom (t, c) = case t of
           Token.NUM (n)   => ((Ast.NUM n), c)
         | Token.VAR (v)   => ((Ast.VAR v), c)
         | Token.LPAREN    =>
               let
                  val (e, c) = getExpression (scan c)
                  val (rp, c) = scan c
               in
                  case rp of
                       Token.RPAREN => (e, c)
                     | _            => raise (Basic.Syntax "expected \")\"")
               end
         | _               => raise (Basic.Syntax "expected expression")

      and getUnary (t, c) = case t of
           Token.MINUS  => let val (a, c) = getAtom (scan c)
                           in (Ast.NEG a, c) end
         | _            => getAtom (t, c)

      and getProduct tk =
      let
         fun loop (left, cont) =
         let
            val (opr, c) = (scan cont)
            fun build f =
               let val (right, c) = getUnary (scan c)
               in loop (f (left, right), c) end
         in
            case opr of
                 Token.MUL => build Ast.MUL
               | Token.DIV => build Ast.DIV
               | _         => (left, cont)
         end
      in
         loop (getUnary tk)
      end

      and getExpression tk =
      let
         fun loop (left, cont) =
         let
            val (opr, c) = (scan cont)
            fun build f =
               let val (right, c) = getProduct (scan c)
               in loop (f (left, right), c) end
         in
            case opr of
                 Token.PLUS   => build Ast.ADD
               | Token.MINUS  => build Ast.SUB
               | _            => (left, cont)
         end
      in
         loop (getProduct tk)
      end

      fun getPrintStm tk =
      let
         fun getItem (t, c) = case t of
              Token.STRING s  => SOME (Ast.STRING s, c)
            | Token.MINUS     => SOME (getExpression (t, c))
            | Token.LPAREN    => SOME (getExpression (t, c))
            | Token.VAR _     => SOME (getExpression (t, c))
            | Token.NUM _     => SOME (getExpression (t, c))
            | _               => NONE

         fun loop (ls, (t, c)) = case getItem (t, c) of
              SOME (x, c)  =>
               let val (sep, c') = scan c
               in case sep of
                    Token.SEMICOLON => loop (Ast.ITEM (x, true)::ls, scan c')
                  | Token.COMMA     => loop (Ast.ITEM (x, false)::ls, scan c')
                  | _               => (Ast.PRINT (List.rev (Ast.ITEM (x, false)::ls)), c)
               end
            | NONE         => (Ast.PRINT (List.rev ls), c)
      in
         loop ([], tk)
      end

      fun getCompare tk =
      let
         val (left, c) = getExpression tk
         val (opr, c) = (scan c)
         val node = case opr of
              Token.EQ => Ast.EQ
            | Token.NE => Ast.NE
            | Token.GT => Ast.GT
            | Token.GE => Ast.GE
            | Token.LT => Ast.LT
            | Token.LE => Ast.LE
            | _        => raise (Basic.Syntax "expected comparison")
         val (right, c) = getExpression (scan c)
      in
         (node (left, right), c)
      end

      fun getGoStm f (t, c) = case t of
           Token.NUM n  => ((f n), c)
         | _            => raise (Basic.Syntax "expected line number")

      fun getFileCmd f (file, c) = case file of
           Token.STRING s  => ((f s), c)
         | _               => raise (Basic.Syntax "expected string")

      fun getVar (t, c) = case t of
           Token.VAR v  => (Ast.VAR v, c)
         | _            => raise (Basic.Syntax "expected variable")

      fun getLetStm tk =
      let
         val (v, c) = getVar tk
         val (t, c) = (scan c)
      in case t of
           Token.EQ  => let val (e, c) = getExpression (scan c)
                        in (Ast.LET ( v, e), c) end
         | _         => raise (Basic.Syntax "expected \"=\"")
      end

      fun getInputStm tk =
      let
         fun loop (ls, c) =
         let
            val (t, c') = (scan c)
         in
            case t of
                 Token.COMMA  => let val (v, c) = getVar (scan c')
                                 in loop (v::ls, c) end
               | _            => (Ast.INPUT (List.rev ls), c)
         end

         val (v, c) = getVar tk
      in
         loop ([v], c)
      end

      fun getRenumCmd c =
      let
         val (t, c') = scan c
      in
         case t of
              Token.NUM m  =>
                  let
                     val (t, c) = scan c'
                     val (t, c) = case t of
                          Token.COMMA => scan c
                        | Token.NUM _ => (t, c)
                        | _           => (Token.EOL, c')
                  in
                     case t of
                          Token.NUM n  => (Ast.RENUM (m, n), c)
                        | _            => (Ast.RENUM (m, 10), c')
                  end
      | _                  => (Ast.RENUM (100, 10), c)
      end

      fun getStatement (t, c) = case t of
           Token.PRINT     => getPrintStm (scan c)
         | Token.EOL       => (Ast.NUL, c)
         | Token.COLON     => (Ast.NUL, c)
         | Token.IF        => getIfStm (scan c)
         | Token.GO        => let val (t, c) = scan c in case t of
                                   Token.TO  => getGoStm Ast.GOTO (scan c)
                                 | Token.SUB => getGoStm Ast.GOSUB (scan c)
                                 | _         => raise (Basic.Syntax "expected GOTO or GOSUB")
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
         | Token.REM (s)   => (Ast.REM s, c)
         | Token.LIST      => (Ast.LIST, c)
         | Token.RUN       => (Ast.RUN, c)
         | Token.BYE       => (Ast.BYE, c)
         | Token.RENUM     => getRenumCmd c
         | _               => raise (Basic.Syntax "expected statement")

      and getCompoundStm tk =
      let
         fun loop (ls, tk) =
         let
            val (s, c) = getStatement tk
            val (t, c') = case s of Ast.NUL => tk | _ => (scan c)
         in
            case t of
                 Token.COLON  => loop (s::ls, (scan c'))
               | Token.EOL    => (case ls of
                                      []     => (s, c)
                                    | x::xs  => (Ast.COMP (s::ls), c) )
               | _            => raise (Basic.Syntax "expected end of line")
         end
      in
         loop ([], tk)
      end

      and getIfStm (t, c) =
      let
         val (tst, c) = getCompare (t, c)
         val (t, c) = (scan c)
         val (stm, c) = case t of
              Token.THEN   => let val (t, c) = scan c in case t of
                                   Token.NUM n  => (Ast.GOTO n, c)
                                 | _            => getCompoundStm (t, c)
                              end
            | _            => raise (Basic.Syntax "expected THEN")
      in
         (Ast.IF (tst, stm), c)
      end

      fun getLine (t, c) = case t of
           Token.NUM (n) =>
               let
                  val (s, _) = getCompoundStm (scan c)
                  handle
                     Basic.Syntax msg => raise
                        (Basic.Syntax (msg ^ " in " ^ Int.toString n))
               in
                  case s of
                       Ast.NUL   => Ast.DEL n
                     | _         => Ast.LINE (n, s)
               end
         | _ => let val (s, _) = getCompoundStm (t, c) in s end

   in
      getLine (scan 0)
   end

   fun parseInput line =
   let
      val scan = Scanner.scanner line

      fun getNumber t = case t of
           Token.NUM n  => n
         | _            => raise (Basic.Input)

      fun getValue (t, c) = case t of
           Token.MINUS     => let
                                 val (t, c) = (scan c)
                                 val n = getNumber t
                              in
                                 (~ n, c)
                              end
         | _               => (getNumber t, c)

      fun getValues (ls, tk) =
      let
         val (n, c) = getValue tk
         val (t, c) = (scan c)
      in
         case t of
              Token.COMMA  => getValues (n::ls, scan c)
            | Token.EOL    => List.rev (n::ls)
            | _            => raise (Basic.Input)
      end

   in
      getValues ([], scan 0)
   end

end
