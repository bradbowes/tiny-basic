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
      | IF of node * node
      | GOTO of node
      | GOSUB of node
      | RETURN
      | INPUT of node list
      | LET of node * node
      | CLEAR
      | LIST
      | RUN
      | END
      | REM of string
      | NUL

   fun toString a =
      let
         fun group a =
            case a of
                 ADD _  => "(" ^ toString a ^ ")"
               | SUB _  => "(" ^ toString a ^ ")"
               | _      => toString a
      in
         case a of
              NUM (n)      => Int.toString n
            | STRING (s)   => "\"" ^ s ^ "\""
            | VAR (v)      => v
            | NEG (n)      => "-" ^ toString n
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
            | PRINT (ls)   => "PRINT " ^ String.concatWith ", " (map toString ls)
            | INPUT (ls)   => "INPUT " ^ String.concatWith ", " (map toString ls)
            | LET (x, y)   => "LET " ^ toString x ^ " = " ^ toString y
            | IF (x, y)    => "IF " ^ toString x ^ " THEN " ^ toString y
            | GOTO (x)     => "GOTO " ^ toString x
            | GOSUB (x)    => "GOSUB " ^ toString x
            | RETURN       => "RETURN"
            | CLEAR        => "CLEAR"
            | END          => "END"
            | REM (s)      => "REM" ^ s
            | NUL          => ""
      (*
            | LIST
            | RUN
      *)
            | _            => "???"
      end


   fun exec (a, prog, env) =
      let
         fun eval a =
            case a of
                 NUM (n)      => n
               | VAR (s)      => let
                                    val v = StrMap.lookup (env, s)
                                 in
                                    case v of
                                         SOME (n)  => n
                                       | NONE      => 0
                                 end
               | NEG (n)      => ~(eval n)
               | ADD (x, y)   => (eval x) + (eval y)
               | SUB (x, y)   => (eval x) - (eval y)
               | MUL (x, y)   => (eval x) * (eval y)
               | DIV (x, y)   => (eval x) div (eval y)
               | _            => raise (Basic.Bug "Can't evaluate non-expression")

         fun compare a =
            case a of
                 EQ (x, y)    => (eval x) = (eval y)
               | NE (x, y)    => (eval x) <> (eval y)
               | GT (x, y)    => (eval x) > (eval y)
               | GE (x, y)    => (eval x) >= (eval y)
               | LT (x, y)    => (eval x) < (eval y)
               | LE (x, y)    => (eval x) <= (eval y)
               | _            => raise (Basic.Bug "Can't compare")

         fun pr ls =
            let
               fun output i =
                  case i of
                       STRING (s)   => s
                     | _            => Int.toString (eval i)
            in
               print (String.concatWith " " (map output ls) ^ "\n")
            end

         fun list () =
            let
               fun ls (line, stm) =
                  print (Int.toString line ^ " " ^ toString stm ^ "\n")
            in
               app ls prog
            end

         fun continue (ls, env) =
            case ls of
                 []     => env
               | x::xs  => let
                              val (_, stm) = x
                           in
                              continue (xs, exec (stm, prog, env))
                           end

         fun run () =
            continue (prog, StrMap.empty)

      in
         case a of
              PRINT (ls)   => (pr ls; env)
            | LET (x, y)   => StrMap.insert (env, toString x, eval y)
            | IF (x, y)    => if compare x then exec (y, prog, env)
                              else env
            | GOTO (x)     => continue (Prog.goto (prog, eval x), env)  

(*
            | INPUT (ls)   =>
            | GOSUB (x)    =>
            | RETURN       =>
      *)
            | CLEAR        => StrMap.empty
            | END          => env
            | REM _        => env
            | NUL          => env
            | RUN          => run ()
            | LIST         => (list (); env)
            | _            => raise Basic.NoImpl
      end

end
