module Side = struct
      type t = Red | Blue | Green | Yellow

      let length = 4
      let empty = Red

      let incr = function
            | Red -> Blue
            | Blue -> Green
            | Green -> Yellow
            | Yellow -> Red

      let int_of = function
            | Red -> 0
            | Blue -> 1
            | Green -> 2
            | Yellow -> 3

      let compare a b =
            let a = int_of a in
            let b = int_of b in
            b - a
end

module Dice = struct
      type t = Fixed of Side.t | Open of Side.t

      let empty = Open Side.empty

      let toggle = function
            | Open side -> Fixed side
            | Fixed side -> Open side

      let shuffle = function
            | Open side ->
                  let rec get_side side = function
                        | 0 -> side
                        | num -> get_side Side.(incr side) (num - 1) in

                  Open (get_side side Random.(int Side.length))
            | Fixed side -> Fixed side
end

module Hand = struct
      type t = Dice.t list

      let empty = Dice.[empty; empty; empty; empty; empty]

      let rec shuffle = function
            | [] -> []
            | dice :: tail -> Dice.(shuffle dice) :: (shuffle tail)

      let toggle position hand =
            let rec go index = function
                  | [] -> []
                  | dice :: tail when index = position -> Dice.(toggle dice) :: go (index + 1) tail
                  | dice :: tail -> dice :: go (index + 1) tail 
            in
            go 0 hand
end

module Counter = struct
      module Accumulator = Map.Make(Side)
end

