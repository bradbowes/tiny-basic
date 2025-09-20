signature SORTABLE =
sig
   type t
   val lt : t * t -> bool
end

signature MAP =
sig
   structure Key : SORTABLE
   type 'a map
   val empty : 'a map
   val insert : 'a map * Key.t * 'a -> 'a map
   val lookup : 'a map * Key.t -> 'a option
end

functor MapFun (structure K : SORTABLE) :> MAP where type Key.t = K.t =
struct
   structure Key : SORTABLE = K

   datatype 'a map =
        LEAF
      | NODE of 'a map * (Key.t * 'a) * 'a map * int

   val empty = LEAF

   fun insert (tbl, key, value) =
      let
         fun height LEAF = 0
           | height (NODE (_, _, _, h)) = h

         fun new_node (l, v, r) =
            NODE (l, v, r, Int.max(height l, height r) + 1)

         fun get_balance LEAF = 0
           | get_balance (NODE (l, _, r, _)) = height l - height r

         fun rotate_left (NODE (l, v, NODE (rl, rv, rr, _), _)) =
               new_node (new_node (l, v, rl), rv, rr)
           | rotate_left _ = raise (Basic.Bug "rotate_left")

         fun rotate_right (NODE (NODE (ll, lv, lr, _), v, r, _)) =
               new_node (ll, lv, new_node (lr, v, r))
           | rotate_right _ = raise (Basic.Bug "rotate_right")

         fun get_key (NODE (_, (k, _), _, _)) = k
           | get_key _ = raise (Basic.Bug "get_key")

         val t = case tbl of
            LEAF => NODE (LEAF, (key, value), LEAF, 1)
          | NODE (l, (k, v), r, h) =>
               if Key.lt (key, k) then
                  new_node (insert (l, key, value), (k, v), r)
               else if Key.lt (k, key) then
                  new_node (l, (k, v), insert (r, key, value))
               else
                  new_node (l, (key, value), r)

         val balance = get_balance t
      in
         case t of
            NODE (l, v, r, h) =>
               if balance > 1 then
                  let
                     val k = get_key l
                  in
                     if Key.lt (key, k) then rotate_right t
                     else if Key.lt (k, key) then rotate_right (NODE (rotate_left l, v, r, h))
                     else t
                  end
               else if balance < ~1 then
                  let
                     val k = get_key r
                  in
                     if Key.lt (k, key) then rotate_left t
                     else if Key.lt (key, k) then rotate_left (NODE (l, v, rotate_right r, h))
                     else t
                  end
               else
                  t
          | _ => raise (Basic.Bug "insert")
      end

   fun lookup (tbl, key) =
      case tbl of
         LEAF => NONE
       | NODE (l, (k, v), r, _) =>
            if Key.lt (key, k) then
               lookup (l, key)
            else if Key.lt (k, key) then
               lookup (r, key)
            else
               SOME v
end

structure Str :> SORTABLE where type t = string =
struct
   type t = string
   val lt = String.<
end

structure StrMap = MapFun (structure K = Str)

structure Num :> SORTABLE where type t = int =
struct
   type t = int
   val lt = op<
end

structure NumMap = MapFun (structure K = Num)
