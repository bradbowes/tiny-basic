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
      let val ch = getChar pos
      in
         if ch = #" " orelse ch = #"\t"
         then skipWhite (pos + 1)
         else (ch, pos)
      end

      fun getNumber pos =
      let
         fun loop (n, pos) =
         let val ch = getChar pos
         in
            if Char.isDigit ch
            then loop (n * 10 + (Char.ord ch - Char.ord #"0"), pos + 1)
            else (NUM (n), pos)
         end
      in loop (0, pos) end

      fun getVar (ls, pos) =
      let val ch = Char.toUpper (getChar pos)
      in
         if Char.isAlphaNum ch
         then getVar (ch :: ls, pos + 1)
         else (VAR (String.implode (List.rev ls)), pos)
      end

      fun getKeyword (tok, kw1, kw2, pos) =
      let
         fun loop (acc, rest, pos) =
         let val ch = Char.toUpper (getChar pos)
         in case rest of
              []    => if Char.isAlphaNum ch
                       then getVar (acc, pos)
                       else (tok, pos)
            | c::cs => if ch = c
                       then loop (c::acc, cs, pos + 1)
                       else getVar (acc, pos)
         end
      in
         loop (List.rev (String.explode kw1), String.explode kw2, pos)
      end

      fun getString pos =
      let
         fun loop (ls, p) =
         let val ch = getChar p
         in
            if ch = #"\"" then
               let val ch = getChar (p + 1)
               in
                  if ch = #"\""
                  then loop (ch::ls, p + 2)
                  else (STRING (String.implode (List.rev ls)), p + 1)
               end
            else if Char.contains "\n\r\^D" ch
            then (STRING (String.implode (List.rev ls)), p + 1)
            else loop (ch::ls, p + 1)
         end
      in loop ([], pos) end

      fun getComment (tok, pos) =
      let
         fun loop (ls, p) =
         let
            val ch = getChar p
            fun ret (ls, p) = (tok (String.implode (List.rev ls)), p)
         in case (ch) of
              #"\n"       => ret (ls, p)
            | #"\r"       => ret (ls, p)
            | #"\^D"      => ret (ls, p)
            | _           => loop (ch::ls, p + 1)
         end
      in loop ([], pos) end

   in
      fn pos =>
      let
         val (ch, p) = skipWhite pos
         fun look n = Char.toUpper (getChar (p + n))
      in case Char.toUpper(ch) of
           #"\^D" => (EOL, p)
         | #"\n"  => (EOL, p + 1)
         | #"\r"  => (EOL, p + (if look 1 = #"\n" then 2 else 1))
         | #"+"   => (PLUS, p + 1)
         | #"-"   => (MINUS, p + 1)
         | #"*"   => (MUL, p + 1)
         | #"/"   => (DIV, p + 1)
         | #"="   => (EQ, p + 1)
         | #"?"   => (PRINT, p + 1)
         | #"'"   => (getComment (TICK, p + 1))
         | #"<"   => let val ch = look 1
                     in case ch of
                          #"="   => (LE, p + 2)
                        | #">"   => (NE, p + 2)
                        | _      => (LT, p + 1)
                     end
         | #">"   => if look 1 = #"="
                     then (GE, p + 2)
                     else (GT, p + 1)
         | #"("   => (LPAREN, p + 1)
         | #")"   => (RPAREN, p + 1)
         | #","   => (COMMA, p + 1)
         | #";"   => (SEMICOLON, p + 1)
         | #":"   => (COLON, p + 1)
         | #"\""  => getString (p + 1)
         | #"B"   => getKeyword (BYE, "B", "YE", p + 1)
         | #"C"   => getKeyword (CLEAR, "C", "LEAR", p + 1)
         | #"E"   => getKeyword (END, "E", "ND", p + 1)
         | #"G"   => if look 1 = #"O" then
                        let val ch = look 2
                        in case ch of
                             #"T"   => getKeyword (GOTO, "GOT", "O", p + 3)
                           | #"S"   => getKeyword (GOSUB, "GOS", "UB", p + 3)
                           | _      => if Char.isAlphaNum ch
                                       then getVar ([ch, #"O", #"G"], p + 3)
                                       else (GO, p + 2)
                        end
                     else getVar ([#"G"], p + 1)
         | #"F"   => getKeyword (FOR, "F", "OR", p + 1)
         | #"I"   => if look 1 = #"F"
                     then getKeyword (IF, "IF", "", p + 2)
                     else  getKeyword (INPUT, "I", "NPUT", p + 1)
         | #"L"   => let val ch = look 1
                     in case ch of
                          #"I"   => getKeyword (LIST, "LI", "ST", p + 2)
                        | #"E"   => getKeyword (LET, "LE", "T", p + 2)
                        | _      => getKeyword (LOAD, "L", "OAD", p + 1)
                     end
         | #"N"   => if look 1 = #"E" then
                        if look 2 = #"X"
                        then getKeyword (NEXT, "NEX", "T", p + 3)
                        else getKeyword (NEW, "NE", "W", p + 2)
                     else getVar ([#"N"], p + 1)
         | #"P"   => getKeyword (PRINT, "P", "RINT", p + 1)
         | #"R"   => let val ch = look 1
                     in case ch of
                          #"U"   => getKeyword (RUN, "RU", "N", p + 2)
                        | #"E"   => let val ch = look 2
                                    in case ch of
                                         #"N"   => getKeyword (RENUM, "REN", "UM", p + 3)
                                       | #"M"   => if not (Char.isAlphaNum (look 3))
                                                   then getComment (REM, p + 3)
                                                   else getVar ([#"M", #"E", #"R"], p + 3)
                                       | _      => getKeyword (RETURN, "RE", "TURN", p + 2)
                                    end
                        |_       => getVar ([#"R"], p + 1)
                     end
         | #"S"   => let val ch = look 1
                     in case ch of
                          #"A"   => getKeyword (SAVE, "SA", "VE", p + 2)
                        | #"T"   => getKeyword (STEP, "ST", "EP", p + 2)
                        | _      => getKeyword (SUB, "S", "UB", p + 1)
                     end
         | #"T"   => if look 1 = #"H"
                     then getKeyword (THEN, "TH", "EN", p + 2)
                     else getKeyword (TO, "T", "O", p + 1)
         | _      =>
            if Char.isDigit ch then getNumber p
            else if Char.isAlpha ch then getVar ([Char.toUpper ch], p + 1)
            else raise (BasicExn.Syntax "Illegal character")
      end
   end
end
