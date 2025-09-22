structure Parser =
struct
   fun parse line =
      let
         val scan = Scanner.make_scanner line

         fun atom (t, c) =
            case t of
                 Token.NUM (n)   => ((Ast.NUM n), c)
               | Token.VAR (v)   => ((Ast.VAR v), c)
               | Token.LPAREN    =>
                     let
                        val (e, c) = expression (scan c)
                        val (rp, c) = scan c
                     in
                        case rp of
                             Token.RPAREN => (e, c)
                           | _            => raise (Basic.Syntax "expected \")\"")
                     end
               | _               => raise (Basic.Syntax "expected expression")

         and unary (t, c) =
            case t of
                 Token.MINUS  => let val (a, c) = atom (scan c)
                                 in (Ast.NEG a, c) end
               | _            => atom (t, c)

         and product tk =
            let
               fun helper (left, cont) =
                  let
                     val (opr, c) = (scan cont)
                     fun fulfill f =
                        let val (right, c) = unary (scan c)
                        in helper (f (left, right), c) end
                  in
                     case opr of
                          Token.MUL => fulfill Ast.MUL
                        | Token.DIV => fulfill Ast.DIV
                        | _         => (left, cont)
                  end
            in
               helper (unary tk)
            end

         and expression tk =
            let
               fun helper (left, cont) =
                  let
                     val (opr, c) = (scan cont)
                     fun fulfill f =
                        let val (right, c) = product (scan c)
                        in helper (f (left, right), c) end
                  in
                     case opr of
                          Token.PLUS   => fulfill Ast.ADD
                        | Token.MINUS  => fulfill Ast.SUB
                        | _            => (left, cont)
                  end
            in
               helper (product tk)
            end

         fun print_stm (t, c) =
            let
               fun item (t, c) =
                  case t of
                       Token.STRING (s)   => (Ast.STRING s, c)
                     | _                  => expression (t, c)

               fun f (ls, c) =
                  let
                     val (t, c') = (scan c)
                  in
                     case t of
                          Token.COMMA  => let val (e, c) = item (scan c')
                                          in f (e::ls, c) end
                        | _            => (Ast.PRINT (List.rev ls), c)
                  end
            in
               case t of
                    Token.EOL => (Ast.PRINT [], c)
                  | Token.EOF => (Ast.PRINT [], c)
                  | _         => let val (e, c) = item (t, c)
                                 in f ([e], c) end
            end

         fun compare tk =
            let
               val (left, c) = expression tk
               val (opr, c) = (scan c)
               val node = case opr of
                                Token.EQ => Ast.EQ
                              | Token.NE => Ast.NE
                              | Token.GT => Ast.GT
                              | Token.GE => Ast.GE
                              | Token.LT => Ast.LT
                              | Token.LE => Ast.LE
                              | _        => raise (Basic.Syntax "expected comparison")
               val (right, c) = expression (scan c)
            in
               (node (left, right), c)
            end

         fun go f tk =
            let
               val (target, c) = expression tk
            in
               ((f target), c)
            end

         fun file_stm f (file, c) =
            case file of
                 Token.STRING s  => ((f s), c)
               | _               => raise (Basic.Syntax "expected string (file path)")

         fun let_stm (t, c) =
            case t of
                 Token.VAR (v)   =>
                     let val (t, c) = (scan c)
                     in case t of
                          Token.EQ  => let val (e, c) = expression (scan c)
                                       in (Ast.LET (Ast.VAR v, e), c) end
                        | _         => raise (Basic.Syntax "expected \"=\"")
                     end
               | _               => raise (Basic.Syntax "expected variable")

         fun input_stm tk =
            let
               fun var (t, c) =
                  case t of
                       Token.VAR v  => (Ast.VAR v, c)
                     | _            => raise (Basic.Syntax "expected variable")

               fun f (ls, c) =
                  let
                     val (t, c') = (scan c)
                  in
                     case t of
                          Token.COMMA  => let val (v, c) = var (scan c')
                                          in f (v::ls, c) end
                        | _            => (Ast.INPUT (List.rev ls), c)
                  end

               val (v, c) = var tk
            in
               f ([v], c)
            end

         fun statement (t, c) =
            case t of
                 Token.PRINT     => print_stm (scan c)
               | Token.EOL       => (Ast.NUL, c)
               | Token.EOF       => (Ast.NUL, c)
               | Token.IF        => if_stm (scan c)
               | Token.GOTO      => go Ast.GOTO (scan c)
               | Token.GOSUB     => go Ast.GOSUB (scan c)
               | Token.RETURN    => (Ast.RETURN, c)
               | Token.CLEAR     => (Ast.CLEAR, c)
               | Token.NEW       => (Ast.NEW, c)
               | Token.LOAD      => file_stm Ast.LOAD (scan c)
               | Token.SAVE      => file_stm Ast.SAVE (scan c)
               | Token.END       => (Ast.END, c)
               | Token.LET       => let_stm (scan c)
               | Token.VAR _     => let_stm (t, c)
               | Token.INPUT     => input_stm (scan c)
               | Token.REM (s)   => (Ast.REM s, c)
               | Token.LIST      => (Ast.LIST, c)
               | Token.RUN       => (Ast.RUN, c)
               | _               => raise (Basic.Syntax "expected statement")

         and if_stm (t, c) =
            let
               val (tst, c) = compare (t, c)
               val (tk, c) = (scan c)
               val (stm, c) = case tk of
                                   Token.THEN   => statement (scan c)
                                 | _            => raise (Basic.Syntax "expected THEN")
            in
               (Ast.IF (tst, stm), c)
            end

         fun line (t, c) =
            case t of
                 Token.NUM (n)   => let
                                       val (s, _) = statement (scan c)
                                    in
                                       case s of
                                            Ast.NUL   => Ast.DEL n
                                          | _         => Ast.LINE (n, s)
                                    end
               | _               => let val (s, _) = statement (t, c) in s end

      in
         line (scan 0)
      end

end
