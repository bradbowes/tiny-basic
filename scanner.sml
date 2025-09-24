structure Scanner : sig
   val scanner : string -> int -> Token.token * int
end =
struct
   open Token
   fun scanner s =
      let
         val sz = size s

         fun getc pos =
            if pos >= sz then #"\^D" else String.sub (s, pos)

         fun whitespace pos =
            let
               val ch = getc pos
            in
               if ch = #" " orelse ch = #"\t" then
                  whitespace (pos + 1)
               else
                  (ch, pos)
            end

         fun number pos =
            let
               fun f (n, pos) =
                  let
                     val ch = getc pos
                  in
                     if Char.isDigit ch then
                        f (n * 10 + (Char.ord ch - Char.ord #"0"), pos + 1)
                     else
                        (NUM (n), pos)
                  end
            in
               f (0, pos)
            end

         fun var (ls, pos) =
            let
               val ch = Char.toUpper (getc pos)
            in
               if Char.isAlphaNum ch then
                  var (ch :: ls, pos + 1)
               else
                  (VAR (String.implode (List.rev ls)), pos)
            end

         fun keyword (tok, kw, pos) =
            let
               val ls = String.explode kw
               fun f (ls, r, pos) =
                  let
                     val ch = Char.toUpper (getc pos)
                  in
                     case ls of
                          []    => if Char.isAlphaNum ch then var (r, pos)
                                   else (tok, pos)
                        | c::cs => if ch = c then
                                      f (cs, c::r, pos + 1)
                                   else var (r, pos)
                  end
            in
               f (ls, [], pos)
            end

         fun str pos =
            let
               fun f (ls, p) =
                  let
                     val ch = getc p
                  in
                     if ch = #"\"" then
                        let
                           val ch = getc (p + 1)
                        in
                           if ch = #"\"" then
                              f (ch::ls, p + 2)
                           else
                              (STRING (String.implode (List.rev ls)), p + 1)
                        end
                     else if Char.contains "\n\r\^D" ch then
                        raise (Basic.Syntax "Unterminated string")
                     else f (ch::ls, p + 1)
                  end
            in
               f ([], pos)
            end

         fun comment pos =
            let
               fun f (ls, p) =
                  let
                     val ch = getc p
                     fun ret (ls, p) = (REM (String.implode (List.rev ls)), p)
                  in
                     case (ch) of
                          #"\n"       => ret (ls, p)
                        | #"\r"       => ret (ls, p)
                        | #"\^D"      => ret (ls, p)
                        | _           => f (ch::ls, p + 1)
                  end
            in
               f ([], pos)
            end

      in
         fn pos =>
            let
               val (ch, p) = whitespace pos
               fun look n ch = Char.toUpper (getc (p + n)) = ch
               val peek = look 1
            in
               case Char.toUpper(ch) of
                    #"\^D" => (EOL, p)
                  | #"\n"  => (EOL, p + 1)
                  | #"\r"  => if peek #"\n" then (EOL, p + 2)
                              else (EOL, p + 1)
                  | #"+"   => (PLUS, p + 1)
                  | #"-"   => (MINUS, p + 1)
                  | #"*"   => (MUL, p + 1)
                  | #"/"   => (DIV, p + 1)
                  | #"="   => (EQ, p + 1)
                  | #"?"   => (PRINT, p + 1)
                  | #"'"   => (comment (p + 1))
                  | #"<"   => let val ch = getc (p + 1) in
                                 if ch = #"=" then (LE, p + 2)
                                 else if ch = #">" then (NE, p + 2)
                                 else (LT, p + 1)
                              end
                  | #">"   => if getc (p + 1) = #"=" then (GE, p + 2)
                              else (GT, p + 1)
                  | #"("   => (LPAREN, p + 1)
                  | #")"   => (RPAREN, p + 1)
                  | #","   => (COMMA, p + 1)
                  | #"\""  => str (p + 1)
                  | #"B"   => keyword (BYE, "BYE", p)
                  | #"C"   => keyword (CLEAR, "CLEAR", p)
                  | #"E"   => keyword (END, "END", p)
                  | #"G"   => if peek #"O" then
                                 if look 2 #"T" then keyword (GOTO, "GOTO", p)
                                 else keyword (GOSUB, "GOSUB", p)
                              else var ([#"G"], p + 1)
                  | #"I"   => if peek #"F" then
                                 keyword (IF, "IF", p)
                              else  keyword (INPUT, "INPUT", p)
                  | #"L"   => if peek #"I" then
                                 keyword (LIST, "LIST", p)
                              else if peek #"E" then keyword (LET, "LET", p)
                              else keyword (LOAD, "LOAD", p)
                  | #"N"   => keyword (NEW, "NEW", p)
                  | #"P"   => keyword (PRINT, "PRINT", p)
                  | #"R"   => if peek #"U" then
                                 keyword (RUN, "RUN", p)
                              else if peek #"E"
                                      andalso look 2 #"M"
                                      andalso not (Char.isAlphaNum (getc (p + 3))) then
                                    comment (p + 3)
                              else keyword (RETURN, "RETURN", p)
                  | #"S"   => keyword (SAVE, "SAVE", p)
                  | #"T"   => keyword (THEN, "THEN", p)
                  | _      =>
                     if Char.isDigit ch then number p
                     else if Char.isAlpha ch then var ([Char.toUpper ch], p + 1)
                     else raise (Basic.Syntax "Illegal character")
            end
      end

end
