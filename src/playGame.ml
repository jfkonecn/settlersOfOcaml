open Types

let getAvailableMoves (game : game) =
  let handleRoundOne () =
    let playerHasSettlement =
      game.gameBoard
      |> List.find_opt (fun x ->
             match x.item with
             | _, Corner (Settlement h) when h = game.currentColor -> true
             | _ -> false)
      |> Option.is_some
    in
    let playerHasRoad =
      game.gameBoard
      |> List.find_opt (fun x ->
             match x.item with
             | _, Edge (Road r) when r = game.currentColor -> true
             | _ -> false)
      |> Option.is_some
    in
    match (playerHasSettlement, playerHasRoad) with
    | true, true -> [ EndTurn ]
    | false, _ -> [ PlaceSettlement ]
    | _, false -> [ PlaceRoad ]
  in

  match game with
  | { round = 1; _ } -> handleRoundOne ()
  | _ -> [ PlaceSettlement ]

let executeIfAllowed move f game =
  let moveIfValid allowedMoves =
    match List.find_opt (( = ) move) allowedMoves with
    | Some _ -> f ()
    | None -> Error (InvalidMove move)
  in

  getAvailableMoves game |> moveIfValid

let placeSettlement id (game : game) =
  let continueWithSettlementPlacement () =
    let currentColor = game.currentColor in
    let rec updateStateOfCorner items =
      match items with
      | { x; y; item = ID cornerId, Corner _ } :: t when cornerId = id ->
          Ok ({ x; y; item = (ID cornerId, Corner (Settlement currentColor)) } :: t)
      | { x = _; y = _; item = ID cornerId, _ } :: _ when cornerId = id ->
          Error (ItemIsNotACorner (ID id))
      | _ :: t -> updateStateOfCorner t
      | [] -> Error (BoardItemNotFound (ID id))
    in

    updateStateOfCorner game.gameBoard
    |> Result.map (fun board -> Ok { game with gameBoard = board })
    |> Result.join
  in
  executeIfAllowed PlaceSettlement continueWithSettlementPlacement game

let listAvailableSettlementLocations (game : game) =
  let toCorner x = match x with id, Corner c -> Some (id, c) | _ -> None in
  game.gameBoard |> List.map (fun x -> x.item) |> List.filter_map toCorner

let listAvailableRoadLocations (game : game) =
  let toEdge x = 
    match x with 
    | id, Edge e -> Some (id, e) 
    | _ -> None 
  in
  game.gameBoard 
  |> List.map (fun x -> x.item) 
  |> List.filter_map toEdge

let getGameItemById id (game : game) =
  let toResult x =
    match x with Some x -> Ok x | None -> Error (BoardItemNotFound (ID id))
  in

  game.gameBoard
  |> List.map (fun x -> x.item)
  |> List.find_opt (fun (ID x, _) -> id = x)
  |> Option.map (fun (_, x) -> x)
  |> toResult
