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

  let raiseToGameGroup f x = ((f x), x) in
  let passGameThrough f (x, y) = ((f x), y) in
  let passGameGroup f (x, y) = (x, (f x y)) in

  start2PlayerValidGame ()
  |> assertAvailableMoves [ PlaceSettlement ]
  |> raiseToGameGroup SettlersOfOcaml.listAvailableSettlementLocaltions
  |> passGameThrough (fun x -> List.nth x 2)
  |> passGameThrough (fun (id, _) -> id)
  |> passGameGroup SettlersOfOcaml.placeSettlement
  |> fun (_, gameResult) -> gameResult
  |> Result.map_error (fun _ -> assert_failure "Failed to place settlement")
  |> Result.get_ok |> assertGameState 1
  |> fun _ -> ()

let tests =
  "test suite for playing game"
  >::: [
         "Round One Should Have Player Place A Settlement And Road"
         >:: roundOneShouldHavePlayerPlaceASettlementAndRoad;
       ]
