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
end
