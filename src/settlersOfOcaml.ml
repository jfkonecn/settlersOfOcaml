module SettlersOfOcaml = struct
  let startWithSeedGame = SetupGame.startWithSeedGame

  let getAvailableMoves = PlayGame.getAvailableMoves

  let placeSettlement = PlayGame.placeSettlement
  let listAvailableSettlementLocaltions = PlayGame.listAvailableSettlementLocaltions
  
  module Types = Types
  module Linq = Linq
end
