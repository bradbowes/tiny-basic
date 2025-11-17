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

      fun newNode (l, v, r) =
         NODE (l, v, r, Int.max(height l, height r) + 1)

      fun getBalance LEAF = 0
        | getBalance (NODE (l, _, r, _)) = height l - height r

      fun rotateLeft (NODE (l, v, NODE (rl, rv, rr, _), _)) =
            newNode (newNode (l, v, rl), rv, rr)
        | rotateLeft _ = raise (BasicExn.Bug "rotateLeft")

      fun rotateRight (NODE (NODE (ll, lv, lr, _), v, r, _)) =
            newNode (ll, lv, newNode (lr, v, r))
        | rotateRight _ = raise (BasicExn.Bug "rotateRight")

      fun getKey (NODE (_, (k, _), _, _)) = k
        | getKey _ = raise (BasicExn.Bug "getKey")

      val t = case tbl of
         LEAF => NODE (LEAF, (key, value), LEAF, 1)
       | NODE (l, (k, v), r, h) =>
            if Key.lt (key, k) then
               newNode (insert (l, key, value), (k, v), r)
            else if Key.lt (k, key) then
               newNode (l, (k, v), insert (r, key, value))
            else
               newNode (l, (key, value), r)

      val balance = getBalance t
   in case t of
        NODE (l, v, r, h) =>
            if balance > 1 then
               let val k = getKey l
               in
                  if Key.lt (key, k) then rotateRight t
                  else if Key.lt (k, key) then rotateRight (NODE (rotateLeft l, v, r, h))
                  else t
               end
            else if balance < ~1 then
               let val k = getKey r
               in
                  if Key.lt (k, key) then rotateLeft t
                  else if Key.lt (key, k) then rotateLeft (NODE (l, v, rotateRight r, h))
                  else t
               end
            else
               t
      | _ => raise (BasicExn.Bug "insert")
   end

   fun lookup (tbl, key) = case tbl of
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

