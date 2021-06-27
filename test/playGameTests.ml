open OUnit2
open SettlersOfOcaml
open SettlersOfOcaml.Types

let startGame = SettlersOfOcaml.startWithSeedGame 1000

let assertAvailableMoves expectedMoves game =
  let moves = SettlersOfOcaml.getAvailableMoves game in
  assert_equal expectedMoves moves;
  game

let forceOk f errMsg =
  f () |> Result.map_error (fun _ -> assert_failure errMsg) |> Result.get_ok

let placeSettlement id game =
  forceOk
    (fun () -> SettlersOfOcaml.placeSettlement id game)
    "Failed to place settlement"

let getGameItemById id game =
  forceOk
    (fun () -> SettlersOfOcaml.getGameItemById id game)
    "Failed to get game item"

let start2PlayerValidGame () =
  let players : playerBlueprint list =
    [ (Name "red", Red); (Name "blue", Blue) ]
  in
  startGame players |> Result.get_ok

let roundOneShouldHavePlayerPlaceASettlement _ =
  let game = start2PlayerValidGame () in
  let curPlayerColor = game.currentColor in
  let assertCanPlaceSettlement (ID id, corner) =
    let itemToCorner x =
      match x with Corner c -> c | _ -> assert_failure "Expected a corner"
    in

    let oldItem = getGameItemById id game |> itemToCorner in
    let game =
      forceOk (fun () -> placeSettlement id game) "Should place settlement"
    in
    let newItem = getGameItemById id game |> itemToCorner in

    assert_equal 1 game.round ~msg:"Should be round 1";
    assert_equal corner oldItem ~msg:"Items should match";
    assert_equal (House curPlayerColor) newItem
      ~msg:"Corner should have settlement";
    ()
  in

  let _ =
    SettlersOfOcaml.listAvailableSettlementLocations game
    |> List.map assertCanPlaceSettlement
  in
  ()

let tests =
  "test suite for playing game"
  >::: [
         "Round One Should Start with a Player Placing A Settlement"
         >:: roundOneShouldHavePlayerPlaceASettlement;
       ]
