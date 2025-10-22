structure Prog =
struct
   fun insert (prog, (ln, stm)) =
   let
      fun loop (acc, rest) = case rest of
           []           => List.revAppend (acc, [(ln, stm)])
         | (l, s)::xs   =>
               if l > ln then List.revAppend (acc, (ln, stm)::rest)
               else if l = ln then List.revAppend (acc, (ln, stm)::xs)
               else loop ((l, s)::acc, xs)
   in
      loop ([], prog)
   end

   fun delete (prog, ln) =
   let
      fun loop (acc, rest) = case rest of
           []           => prog
         | (l, s)::xs   =>
               if l > ln then prog
               else if l = ln then List.revAppend (acc, xs)
               else loop ((l, s)::acc, xs)
   in
      loop ([], prog)
   end

   fun goto (continue, ln) = case continue of
        []           => raise Basic.NoLine
      | (l, _)::xs   =>
            if l > ln then raise Basic.NoLine
            else if l = ln then map #2 continue
            else goto (xs, ln)

   fun renum (prog, start, inc) =
   let
      fun mapLines (map, rest, n) = case rest of
           []           => map
         | (l, _)::xs   => mapLines (NumMap.insert (map, l, n), xs, n + inc)

      val lines = mapLines (NumMap.empty, prog, start)

      fun newTarget n =
         getOpt (NumMap.lookup (lines, n), n)

      fun fix s = case s of
           Ast.GOTO n      => Ast.GOTO (newTarget n)
         | Ast.GOSUB n     => Ast.GOSUB (newTarget n)
         | Ast.IF (c, s)   => Ast.IF (c, fix s)
         | Ast.COMP ls     => Ast.COMP (map fix ls)
         | _               => s

      fun loop (acc, rest, n) = case rest of
           []           => List.rev acc
         | (_, s)::xs   => loop ((n, (fix s))::acc, xs, n + inc)

   in
      loop ([], prog, start)
   end

end
