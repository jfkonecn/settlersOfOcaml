module SettlersOfOcaml = struct
  let rec sum = function
  | []    -> 0
  | x::xs -> x + sum xs
  let startGame = SetupGame.startGame

  module Types = Types
end


