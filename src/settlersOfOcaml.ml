module SettlersOfOcaml = struct
  let startWithSeedGame = SetupGame.startWithSeedGame

  let getAvailableMoves = PlayGame.getAvailableMoves

  let placeSettlement = PlayGame.placeSettlement

  module Types = Types
  module Linq = Linq
end
