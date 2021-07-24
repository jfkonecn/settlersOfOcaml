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

let standard2PlayerGame =
  let players : playerBlueprint list =
    [ (Name "red", Red); (Name "blue", Blue) ]
  in
  startGame players |> Result.get_ok

let roundOneShouldHavePlayerPlaceASettlement _ =
  let game = standard2PlayerGame in
  let curPlayerColor = game.currentColor in
  let assertCanPlaceSettlement (ID id, corner) =
    let itemToCorner x =
      match x with Corner c -> c | _ -> assert_failure "Expected a corner"
    in

    let oldItem = getGameItemById id game |> itemToCorner in
    let game = placeSettlement id game in
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

let roundOneShouldForcePlayerToPlaceARoadAfterASettlement _ =
  let stateBind f (x, s) =
    let y = f (x, s) in
    (y, s)
  in

  let game =
    ((), standard2PlayerGame)
    |> stateBind (fun (_, s) ->
           SettlersOfOcaml.listAvailableSettlementLocations s)
    |> (fun x ->
         match x with
         | (ID id, _) :: _, game -> SettlersOfOcaml.placeSettlement id game
         | _ -> assert_failure "Should be able to settle somewhere")
    |> Result.get_ok
  in

  match game |> SettlersOfOcaml.getAvailableMoves with
  | [ PlaceRoad ] -> ()
  | _ -> assert_failure "Should only be allowed to place a road"

let tests =
  "test suite for playing game"
  >::: [
         "Round One Should Start with a Player Placing A Settlement"
         >:: roundOneShouldHavePlayerPlaceASettlement;
         "Round One Should Force Player To Place A Road After A Settlement"
         >:: roundOneShouldForcePlayerToPlaceARoadAfterASettlement;
       ]
