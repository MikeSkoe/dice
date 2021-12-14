module type T = sig
    type t

    val empty: t
end

module Dice = struct
      type t = Red | Blue | Green | Yellow

      let length = 4
      let empty = Red

      let incr = function
            | Red -> Blue
            | Blue -> Green
            | Green -> Yellow
            | Yellow -> Red

      let shuffle t =
            let random_shift = Random.(int length) in
            let rec get_side side = function
                  | 0 -> side
                  | num -> get_side (incr side) (num -1)
            in
            get_side t random_shift

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

module Fixable = struct
      type 'a t = Fixed of 'a | Open of 'a

      let toggle = function
            | Open a -> Fixed a
            | Fixed a -> Open a

      let update fn = function
            | Open a -> Open (fn a)
            | Fixed a -> Fixed a

      let extract = function 
            | Open a
            | Fixed a -> a
end

module Hand = struct
      type t = Dice.t Fixable.t list

      let empty = Fixable.[
          Open Dice.empty;
          Open Dice.empty;
          Open Dice.empty;
          Open Dice.empty;
          Open Dice.empty;
      ]

      let rec shuffle = function
            | [] -> []
            | dice :: tail -> Fixable.update Dice.shuffle dice :: (shuffle tail)

      let toggle position hand =
            let rec go index = function
                  | [] -> []
                  | dice :: tail when index = position -> Fixable.update Dice.shuffle dice :: go (index + 1) tail
                  | dice :: tail -> dice :: go (index + 1) tail 
            in
            go 0 hand
end

module Opt = struct
    let map fn = function
        | None -> None
        | Some a -> Some (fn a)
end

module Counter = struct
      module Accumulator = Map.Make(Dice)

      let empty = Accumulator.(
          empty
          |> add Dice.Red 0
          |> add Dice.Blue 0
          |> add Dice.Green 0
          |> add Dice.Yellow 0
      )

      let collect hand =
          let rec go acc = function
              | [] -> acc
              | dice :: hand -> 
                      let opt_incr = Opt.map ((+) 1) in
                      let key = Fixable.extract dice in
                      go (Accumulator.update key opt_incr acc) hand
          in go empty hand
end

