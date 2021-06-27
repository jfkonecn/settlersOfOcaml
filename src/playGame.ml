open Types

let getAvailableMoves (_ : game) = [ PlaceSettlement ]

let executeIfAllowed move f game =
  let isMatch x y = x = y in
  let moveIfValid allowedMoves =
    match List.find_opt (isMatch move) allowedMoves with
    | Some _ -> f ()
    | None -> Error (InvalidMove move)
  in

  getAvailableMoves game |> moveIfValid

let placeSettlement _ (game : game) =
  let continueWithSettlementPlacement () = Ok game in
  executeIfAllowed PlaceSettlement continueWithSettlementPlacement game

let listAvailableSettlementLocaltions (game : game) =
  let toCorner x =
    match x with
    | id, Corner c -> Some (id, c)
    | _ -> None 
  in
  game.gameBoard
  |> List.map (fun x -> x.item)
  |> List.filter_map toCorner
