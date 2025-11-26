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
      | GOTO of int
      | GOSUB of int
      | RETURN
      | FOR of string * node * node * node option
      | NEXT of string option
      | INPUT of string option * string list
      | LET of string * node
      | CLEAR
      | NEW
      | LOAD of string
      | SAVE of string
      | LIST
      | RUN of string option
      | END
      | REM of string
      | TICK of string
      | LINE of int * node
      | DEL of int
      | NUL
      | BYE
      | RENUM of int * int
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

      fun prItems (ls, s) = case ls of
           []              => s
         | ITEM (i, j)::[] => s ^ (toString i) ^ (if j then ";" else "")
         | ITEM (i, j)::xs => prItems (
                                 xs,
                                 s ^ (toString i) ^ (if j then "; " else ", "))
         | _               => raise (BasicExn.Bug "expected print item")

      fun prCompound (ls, s) = case ls of
           []           => s
         | x::TICK r::_ => s ^ toString x ^ " '" ^ r
         | x::[]        => s ^ toString x
         | x::xs        => prCompound (xs, s ^ toString x ^ ": ")

      fun prompt p = case p of
           SOME s => toString (STRING s) ^ ", "
         | NONE   => ""

   in
      case a of
           NUM n        => Int.toString n
         | STRING s     => bstr s
         | VAR v        => v
         | NEG n        => "-" ^ toString n
         | ADD (x, y)   => toString x ^ " + " ^ toString y
         | SUB (x, y)   => toString x ^ " - " ^ toString y
         | MUL (x, y)   => group x ^ "*" ^ group y
         | DIV (x, y)   => group x ^ "/" ^ group y
         | EQ (x, y)    => toString x ^ " = " ^ toString y
         | NE (x, y)    => toString x ^ " <> " ^ toString y
         | GT (x, y)    => toString x ^ " > " ^ toString y
         | GE (x, y)    => toString x ^ " >= " ^ toString y
         | LT (x, y)    => toString x ^ " < " ^ toString y
         | LE (x, y)    => toString x ^ " <= " ^ toString y
         | PRINT ls     => "PRINT " ^ prItems (ls, "")
         | INPUT (p, ls)=> "INPUT " ^ prompt p ^ String.concatWith ", " ls
         | LET (x, y)   => "LET " ^ x ^ " = " ^ toString y
         | IF (x, y)    => "IF " ^ toString x ^ " THEN " ^ toString y
         | GOTO n       => "GOTO " ^ Int.toString n
         | GOSUB n      => "GOSUB " ^ Int.toString n
         | RETURN       => "RETURN"
         | FOR
           (w, x, y, z) => "FOR " ^ w ^ " = " ^ toString x ^ " TO " ^ toString y ^
                           (case z of SOME e => " STEP " ^ toString e | NONE => "")
         | NEXT x       => "NEXT " ^ getOpt (x, "")
         | CLEAR        => "CLEAR"
         | NEW          => "NEW"
         | LOAD s       => "LOAD " ^ bstr s
         | SAVE s       => "SAVE " ^ bstr s
         | END          => "END"
         | REM s        => "REM" ^ s
         | TICK s       => "'" ^ s
         | LIST         => "LIST"
         | RUN f        => "RUN" ^
                           (case f of SOME s => " " ^ toString (STRING s) | NONE => "")
         | BYE          => "BYE"
         | RENUM (m, n) => "RENUM " ^ Int.toString m ^ ", " ^ Int.toString n
         | COMP ls      => prCompound (ls, "")
         | _            => ""
   end
end
