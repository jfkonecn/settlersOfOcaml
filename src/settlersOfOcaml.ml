module SettlersOfOcaml = struct
  let startWithSeedGame = SetupGame.startWithSeedGame

  let getAvailableMoves = PlayGame.getAvailableMoves

  let placeSettlement = PlayGame.placeSettlement
  let listAvailableSettlementLocations = PlayGame.listAvailableSettlementLocations
  let listAvailableRoadLocations = PlayGame.listAvailableRoadLocations
  let getGameItemById = PlayGame.getGameItemById
  
  module Types = Types
  module Linq = Linq
end
