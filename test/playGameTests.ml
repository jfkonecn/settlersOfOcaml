open OUnit2
open SettlersOfOcaml
open SettlersOfOcaml.Types
open DebugHelpers

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

let getPointById id game =
  game.gameBoard 
  |> List.find (fun point -> 
    let (ID id', _) = point.item in
    id' = id) 

let standard2PlayerGame =
  let players : playerBlueprint list =
    [ (Name "red", Red); (Name "blue", Blue) ]
  in
  startGame players |> Result.get_ok

let placeSettlementSomewhere =
  Stateful.return ()
  |> Stateful.bindPeek SettlersOfOcaml.listAvailableSettlementLocations
  |> Stateful.mapWithState (fun x ->
         (match x with
         | (ID id, _) :: _, game -> SettlersOfOcaml.placeSettlement id game
         | _ -> assert_failure "Should be able to settle somewhere")
         |> Result.get_ok
         |> fun x -> ((), x))

let roundOneShouldHavePlayerPlaceASettlement _ =
  let game = standard2PlayerGame in
  let curPlayerColor = game.currentColor in
  let assertCanPlaceSettlement (ID id, corner) =
    let itemToCorner x =
      match x with Corner c -> c | _ -> assert_failure "Expected a corner"
    in

    let oldItem = getGameItemById id game |> itemToCorner in
    let newGame = placeSettlement id game in
    let newItem = getGameItemById id newGame |> itemToCorner in

    assert_equal 1 game.round ~msg:"Should be round 1";
    assert_equal corner oldItem ~msg:"Items should match";
    assert_equal (Settlement curPlayerColor) newItem
      ~msg:"Corner should have settlement";
    ()
  in

  let _ =
    match game |> SettlersOfOcaml.getAvailableMoves with
    | [ PlaceSettlement ] -> ()
    | _ -> assert_failure "Should only be allowed to place a settlement"
  in

  SettlersOfOcaml.listAvailableSettlementLocations game
  |> List.map assertCanPlaceSettlement
  |> ignore

let roundOneShouldForcePlayerToPlaceARoadAfterASettlement _ =
  let _, game = placeSettlementSomewhere |> Stateful.run standard2PlayerGame in

  match game |> SettlersOfOcaml.getAvailableMoves with
  | [ PlaceRoad ] -> ()
  | _ -> assert_failure "Should only be allowed to place a road"

let playerShouldOnlyBeAllowedToPlaceARoadOnEdgeNextToTheirSettlement _ =
  let game = standard2PlayerGame in
  let assertRoadLocations (ID cornerId, _) =
    let newGame = placeSettlement cornerId game in
    let cornerPoint = getPointById cornerId newGame in
    let assertEdgeIsNextToCorner (ID edgeId , _) =
      let edgePoint = getPointById edgeId newGame in
      let deltaX = abs_float (edgePoint.x -. cornerPoint.x) in
      let deltaY = abs_float (edgePoint.y -. cornerPoint.y) in
      let isNextToCorner = deltaX < 0.8 && deltaY < 0.8 in
      print_gameBoard_point cornerPoint;
      print_gameBoard_point edgePoint;
      assert_bool "Edge should be next to corner" isNextToCorner;
    in
    newGame
    |> SettlersOfOcaml.listAvailableRoadLocations
    |> List.iter assertEdgeIsNextToCorner
  in

  let _ =
    match game |> SettlersOfOcaml.getAvailableMoves with
    | [ PlaceSettlement ] -> ()
    | _ -> assert_failure "Should only be allowed to place a settlement"
  in

  SettlersOfOcaml.listAvailableSettlementLocations game
  |> List.map assertRoadLocations
  |> ignore

let tests =
  "test suite for playing game"
  >::: [
         "Round One Should Start with a Player Placing A Settlement"
         >:: roundOneShouldHavePlayerPlaceASettlement;
         "Round One Should Force Player To Place A Road After A Settlement"
         >:: roundOneShouldForcePlayerToPlaceARoadAfterASettlement;
         "Player Should Only Be Allowed To Place A Road On Edge Next To Their \
          Settlement"
         >:: playerShouldOnlyBeAllowedToPlaceARoadOnEdgeNextToTheirSettlement;
       ]
