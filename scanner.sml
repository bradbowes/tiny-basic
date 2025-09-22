structure Scanner : sig
   val make_scanner : string -> int -> Token.token * int
end =
struct
   open Token
   fun make_scanner s =
      let
         val sz = size s

         fun get_char pos =
            if pos >= sz then #"\^D" else String.sub (s, pos)

         fun skip_white pos =
            let
               val ch = get_char pos
            in
               if ch = #" " orelse ch = #"\t" then
                  skip_white (pos + 1)
               else
                  (ch, pos)
            end

         fun get_number pos =
            let
               fun f (n, pos) =
                  let
                     val ch = get_char pos
                  in
                     if Char.isDigit ch then
                        f (n * 10 + (Char.ord ch - Char.ord #"0"), pos + 1)
                     else
                        (NUM (n), pos)
                  end
            in
               f (0, pos)
            end

         fun get_var (ls, pos) =
            let
               val ch = Char.toUpper (get_char pos)
            in
               if Char.isAlphaNum ch then
                  get_var (ch :: ls, pos + 1)
               else
                  (VAR (String.implode (List.rev ls)), pos)
            end

         fun get_kw (tok, kw, pos) =
            let
               val ls = String.explode kw
               fun f (ls, r, pos) =
                  let
                     val ch = Char.toUpper (get_char pos)
                  in
                     case ls of
                          []    => if Char.isAlphaNum ch then get_var (r, pos)
                                   else (tok, pos)
                        | c::cs => if ch = c then
                                      f (cs, c::r, pos + 1)
                                   else get_var (r, pos)
                  end
            in
               f (ls, [], pos)
            end

         fun get_string pos =
            let
               fun f (ls, p) =
                  let
                     val ch = get_char p
                     fun done (p) = (STRING (String.implode (List.rev ls)), p)
                  in
                     if ch = #"\"" then
                        let
                           val ch = get_char (p + 1)
                        in
                           if ch = #"\"" then
                              f (ch::ls, p + 2)
                           else
                              done (p + 1)
                        end
                     else if Char.contains "\n\r\^D" ch then
                        done (p)
                     else f (ch::ls, p + 1)
                  end
            in
               f ([], pos)
            end

         fun get_comment pos =
            let
               fun f (ls, p) =
                  let
                     val ch = get_char p
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
               val (ch, p) = skip_white pos
               fun look n ch = Char.toUpper (get_char (p + n)) = ch
               val peek = look 1
            in
               case Char.toUpper(ch) of
                    #"\^D" => (EOF, p)
                  | #"\n"  => (EOL, p + 1)
                  | #"\r"  => if peek #"\n" then (EOL, p + 2)
                              else (EOL, p + 1)
                  | #"+"   => (PLUS, p + 1)
                  | #"-"   => (MINUS, p + 1)
                  | #"*"   => (MUL, p + 1)
                  | #"/"   => (DIV, p + 1)
                  | #"="   => (EQ, p + 1)
                  | #"?"   => (PRINT, p + 1)
                  | #"'"   => (get_comment (p + 1))
                  | #"<"   => let val ch = get_char (p + 1) in
                                 if ch = #"=" then (LE, p + 2)
                                 else if ch = #">" then (NE, p + 2)
                                 else (LT, p + 1)
                              end
                  | #">"   => if get_char (p + 1) = #"=" then (GE, p + 2)
                              else (GT, p + 1)
                  | #"("   => (LPAREN, p + 1)
                  | #")"   => (RPAREN, p + 1)
                  | #","   => (COMMA, p + 1)
                  | #"\""  => get_string (p + 1)
                  | #"C"   => get_kw (CLEAR, "CLEAR", p)
                  | #"E"   => get_kw (END, "END", p)
                  | #"G"   => if peek #"O" then
                                 if look 2 #"T" then get_kw (GOTO, "GOTO", p)
                                 else get_kw (GOSUB, "GOSUB", p)
                              else get_var ([#"G"], p + 1)
                  | #"I"   => if peek #"F" then
                                 get_kw (IF, "IF", p)
                              else  get_kw (INPUT, "INPUT", p)
                  | #"L"   => if peek #"I" then
                                 get_kw (LIST, "LIST", p)
                              else if peek #"E" then get_kw (LET, "LET", p)
                              else get_kw (LOAD, "LOAD", p)
                  | #"N"   => get_kw (NEW, "NEW", p)
                  | #"P"   => get_kw (PRINT, "PRINT", p)
                  | #"R"   => if peek #"U" then
                                 get_kw (RUN, "RUN", p)
                              else if peek #"E"
                                      andalso look 2 #"M"
                                      andalso not (Char.isAlphaNum (get_char (p + 3))) then
                                    get_comment (p + 3)
                              else get_kw (RETURN, "RETURN", p)
                  | #"S"   => get_kw (SAVE, "SAVE", p)
                  | #"T"   => get_kw (THEN, "THEN", p)
                  | _      =>
                     if Char.isDigit ch then get_number p
                     else if Char.isAlpha ch then get_var ([Char.toUpper ch], p + 1)
                     else raise (Basic.Syntax "Illegal character")
            end
      end

end
