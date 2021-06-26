open OUnit2
open SettlersOfOcaml
open SettlersOfOcaml.Types

let startGame = SettlersOfOcaml.startWithSeedGame 1000

let start2PlayerValidGame () =
  let players : playerBlueprint list =
    [ (Name "red", Red); (Name "blue", Blue) ]
  in
  startGame players |> Result.get_ok

let roundOneShouldHavePlayerPlaceASettlementAndRoad _ =
  let assertAvailableMoves expectedMoves game =
    let moves = SettlersOfOcaml.getAvailableMoves game in
    assert_equal expectedMoves moves;
    game
  in

  let assertGameState round game =
    assert_equal round game.round;
    game
  in

  start2PlayerValidGame ()
  |> assertAvailableMoves [ PlaceSettlement ]
  |> SettlersOfOcaml.placeSettlement
  |> Result.map_error (fun _ -> assert_failure "Failed to place settlement")
  |> Result.get_ok |> assertGameState 1
  |> fun _ -> ()

let tests =
  "test suite for playing game"
  >::: [
         "Round One Should Have Player Place A Settlement And Road"
         >:: roundOneShouldHavePlayerPlaceASettlementAndRoad;
       ]
