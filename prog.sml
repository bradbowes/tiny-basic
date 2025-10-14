structure Prog =
struct
   fun insert (prog, (ln, stm)) =
   let
      fun loop (prev, next) = case next of
           []           => List.revAppend (prev, [(ln, stm)])
         | (l, s)::xs   =>
               if l > ln then List.revAppend (prev, (ln, stm)::next)
               else if l = ln then List.revAppend (prev, (ln, stm)::xs)
               else loop ((l, s)::prev, xs)
   in
      loop ([], prog)
   end

   fun delete (prog, ln) =
   let
      fun loop (prev, next) = case next of
           []           => prog
         | (l, s)::xs   =>
               if l > ln then prog
               else if l = ln then List.revAppend (prev, xs)
               else loop ((l, s)::prev, xs)
   in
      loop ([], prog)
   end

   fun goto (prog, ln) = case prog of
        []           => raise Basic.NoLine
      | (l, _)::xs   =>
            if l > ln then raise Basic.NoLine
            else if l = ln then map #2 prog
            else goto (xs, ln)
end
