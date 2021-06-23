module SettlersOfOcaml = struct
  let rec sum = function [] -> 0 | x :: xs -> x + sum xs

  let startWithSeedGame = SetupGame.startWithSeedGame

  module Types = Types
end
