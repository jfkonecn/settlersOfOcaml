module SettlersOfOcaml = struct
  let startWithSeedGame = SetupGame.startWithSeedGame
  let getAvailableMoves = PlayGame.getAvailableMoves
  let makeMove = PlayGame.makeMove
  module Types = Types
end
