structure Token =
struct
   datatype token
      = EOF
      | EOL
      | PLUS
      | MINUS
      | MUL
      | DIV
      | EQ
      | LT
      | LE
      | GT
      | GE
      | NE
      | LPAREN
      | RPAREN
      | COMMA
      | IF
      | THEN
      | END
      | LET
      | GOTO
      | GOSUB
      | RETURN
      | INPUT
      | PRINT
      | CLEAR
      | LIST
      | RUN
      | NUM of int
      | VAR of string
      | STRING of string
      | REM of string

   fun toString t =
      "[" ^ (
      case t of
           EOF         => "EOF"
         | EOL         => "EOL"
         | PLUS        => "PLUS"
         | MINUS       => "MINUS"
         | MUL         => "MUL"
         | DIV         => "DIV"
         | EQ          => "EQ"
         | LT          => "LT"
         | LE          => "LE"
         | GT          => "GT"
         | GE          => "GE"
         | NE          => "NE"
         | LPAREN      => "LPAREN"
         | RPAREN      => "RPAREN"
         | COMMA       => "COMMA"
         | IF          => "IF"
         | THEN        => "THEN"
         | END         => "END"
         | LET         => "LET"
         | GOTO        => "GOTO"
         | GOSUB       => "GOSUB"
         | RETURN      => "RETURN"
         | INPUT       => "INPUT"
         | PRINT       => "PRINT"
         | CLEAR       => "CLEAR"
         | LIST        => "LIST"
         | RUN         => "RUN"
         | NUM (n)     => "NUM:" ^ Int.toString(n)
         | VAR (s)     => "VAR:" ^ s
         | STRING (s)  => "STRING:" ^ s
         | REM (s)     => "REM:" ^ s
      ) ^ "]"
end
