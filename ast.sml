structure Ast =
struct
   datatype node =
        NUM of int
      | STRING of string
      | VAR of string
      | NEG of node
      | ADD of node * node
      | SUB of node * node
      | MUL of node * node
      | DIV of node * node
      | EQ of node * node
      | NE of node * node
      | GT of node * node
      | GE of node * node
      | LT of node * node
      | LE of node * node
      | PRINT of node list
      | ITEM of node * bool
      | IF of node * node
      | GOTO of node
      | GOSUB of node
      | RETURN
      | INPUT of node list
      | LET of node * node
      | CLEAR
      | NEW
      | LOAD of string
      | SAVE of string
      | LIST
      | RUN
      | END
      | REM of string
      | LINE of int * node
      | DEL of int
      | NUL
      | BYE
      | COMP of node list

   fun toString a =
   let
      fun group a = case a of
           ADD _  => "(" ^ toString a ^ ")"
         | SUB _  => "(" ^ toString a ^ ")"
         | _      => toString a

      fun bstr s =
         "\"" ^
         (String.translate (fn c => if c = #"\"" then "\"\"" else str c) s) ^
         "\""

      fun prItems (ls, s) =
         case ls of
              []                 => s
            | ITEM (i, j)::[]  => s ^ (toString i) ^ (if j then ";" else "")
            | ITEM (i, j)::xs  => prItems (
                                       xs,
                                       s ^ (toString i) ^ (if j then "; " else ", "))
            | _                  => raise (Basic.Bug "expected print item")
   in
      case a of
           NUM n        => Int.toString n
         | STRING s     => bstr s
         | VAR v        => v
         | NEG n        => "-" ^ toString n
         | ADD (x, y)   => toString x ^ " + " ^ toString y
         | SUB (x, y)   => toString x ^ " - " ^ toString y
         | MUL (x, y)   => group x ^ " * " ^ group y
         | DIV (x, y)   => group x ^ " / " ^ group y
         | EQ (x, y)    => toString x ^ " = " ^ toString y
         | NE (x, y)    => toString x ^ " <> " ^ toString y
         | GT (x, y)    => toString x ^ " > " ^ toString y
         | GE (x, y)    => toString x ^ " >= " ^ toString y
         | LT (x, y)    => toString x ^ " < " ^ toString y
         | LE (x, y)    => toString x ^ " <= " ^ toString y
         | PRINT ls     => "PRINT " ^ prItems (ls, "")
         | INPUT ls     => "INPUT " ^ String.concatWith ", " (map toString ls)
         | LET (x, y)   => "LET " ^ toString x ^ " = " ^ toString y
         | IF (x, y)    => "IF " ^ toString x ^ " THEN " ^ toString y
         | GOTO x       => "GOTO " ^ toString x
         | GOSUB x      => "GOSUB " ^ toString x
         | RETURN       => "RETURN"
         | CLEAR        => "CLEAR"
         | NEW          => "NEW"
         | LOAD s       => "LOAD " ^ bstr s
         | SAVE s       => "SAVE " ^ bstr s
         | END          => "END"
         | REM s        => "REM" ^ s
         | LIST         => "LIST"
         | RUN          => "RUN"
         | BYE          => "BYE"
         | COMP ls      => String.concatWith ": " (map toString (List.rev ls))
         | _            => ""
   end
end
