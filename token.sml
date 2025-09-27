structure Token =
struct
   datatype token
      = EOL
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
      | SEMICOLON
      | COLON
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
      | NEW
      | LOAD
      | SAVE
      | LIST
      | RUN
      | NUM of int
      | VAR of string
      | STRING of string
      | REM of string
      | BYE
end
