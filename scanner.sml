structure Scanner : sig
   val scanner : string -> int -> Token.token * int
end =
struct
   open Token
   fun scanner s =
   let
      val sz = size s

      fun getChar pos =
         if pos >= sz then #"\^D" else String.sub (s, pos)

      fun skipWhite pos =
      let
         val ch = getChar pos
      in
         if ch = #" " orelse ch = #"\t" then
            skipWhite (pos + 1)
         else
            (ch, pos)
      end

      fun number pos =
      let
         fun loop (n, pos) =
         let
            val ch = getChar pos
         in
            if Char.isDigit ch then
               loop (n * 10 + (Char.ord ch - Char.ord #"0"), pos + 1)
            else
               (NUM (n), pos)
         end
      in
         loop (0, pos)
      end

      fun var (ls, pos) =
      let
         val ch = Char.toUpper (getChar pos)
      in
         if Char.isAlphaNum ch then
            var (ch :: ls, pos + 1)
         else
            (VAR (String.implode (List.rev ls)), pos)
      end

      fun keyword (tok, kw, pos) =
      let
         val ls = String.explode kw
         fun loop (ls, r, pos) =
         let
            val ch = Char.toUpper (getChar pos)
         in
            case ls of
                 []    => if Char.isAlphaNum ch then var (r, pos)
                          else (tok, pos)
               | c::cs => if ch = c then
                             loop (cs, c::r, pos + 1)
                          else var (r, pos)
         end
      in
         loop (ls, [], pos)
      end

      fun string pos =
      let
         fun loop (ls, p) =
         let
            val ch = getChar p
         in
            if ch = #"\"" then
               let
                  val ch = getChar (p + 1)
               in
                  if ch = #"\"" then
                     loop (ch::ls, p + 2)
                  else
                     (STRING (String.implode (List.rev ls)), p + 1)
               end
            else if Char.contains "\n\r\^D" ch then
               raise (Basic.Syntax "Unterminated string")
            else loop (ch::ls, p + 1)
         end
      in
         loop ([], pos)
      end

      fun comment pos =
      let
         fun loop (ls, p) =
         let
            val ch = getChar p
            fun ret (ls, p) = (REM (String.implode (List.rev ls)), p)
         in
            case (ch) of
                 #"\n"       => ret (ls, p)
               | #"\r"       => ret (ls, p)
               | #"\^D"      => ret (ls, p)
               | _           => loop (ch::ls, p + 1)
         end
      in
         loop ([], pos)
      end

   in
      fn pos =>
      let
         val (ch, p) = skipWhite pos
         fun look n ch = Char.toUpper (getChar (p + n)) = ch
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
            | #"<"   => if peek #"=" then (LE, p + 2)
                        else if peek #">" then (NE, p + 2)
                        else (LT, p + 1)
            | #">"   => if peek #"=" then (GE, p + 2)
                        else (GT, p + 1)
            | #"("   => (LPAREN, p + 1)
            | #")"   => (RPAREN, p + 1)
            | #","   => (COMMA, p + 1)
            | #";"   => (SEMICOLON, p + 1)
            | #":"   => (COLON, p + 1)
            | #"\""  => string (p + 1)
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
            | #"L"   => if peek #"I" then keyword (LIST, "LIST", p)
                        else if peek #"E" then keyword (LET, "LET", p)
                        else keyword (LOAD, "LOAD", p)
            | #"N"   => keyword (NEW, "NEW", p)
            | #"P"   => keyword (PRINT, "PRINT", p)
            | #"R"   => if peek #"U" then keyword (RUN, "RUN", p)
                        else if peek #"E"
                              andalso look 2 #"M"
                              andalso not (Char.isAlphaNum (getChar (p + 3))) then
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
