module Side = struct
      type t = Red | Blue | Green | Yellow

      let length = 4

      let incr = function
            | Red -> Blue
            | Blue -> Green
            | Green -> Yellow
            | Yellow -> Red
end

module Dice = struct
      type t = Fixed of Side.t | Open of Side.t

      let fix = function
            | Open side -> Fixed side
            | Fixed side -> Fixed side

      let unfix = function
            | Open side -> Open side
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
end

