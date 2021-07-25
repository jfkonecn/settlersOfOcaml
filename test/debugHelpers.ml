open SettlersOfOcaml
open SettlersOfOcaml.Types

let circularToken_to_string (token : circularToken) =
  let colorStr = match token.color with Red -> "<Red>" | Black -> "<Black>" in
  Printf.sprintf "{ value : %i; color : %s; letter : %c }" token.value colorStr token.letter

let terrain_to_string (terrain : terrain) =
  match terrain with
  | Hills -> "<Hills>"
  | Forest -> "<Forest>"
  | Mountains -> "<Mountains>"
  | Fields -> "<Fields>"
  | Pasture -> "<Pasture>"
  | Desert -> "<Desert>"

let resourceCard_to_string (card : resourceCard) =
  match card with
  | Brick -> "<Brick>"
  | Grain -> "<Grain>"
  | Lumber -> "<Lumber>"
  | Ore -> "<Ore>"
  | Wool -> "<Wool>"

let playerColor_to_string (color : playerColor) =
  match color with
  | Red -> "<Red>"
  | White -> "<White>"
  | Orange -> "<Orange>"
  | Blue -> "<Blue>"

let terrainTile_to_string (title : terrainTile) =
  let robberOpt_to_string (robberOpt : robber option) =
    match robberOpt with Some Robber -> "<Robber>" | None -> "<None>"
  in
  match title with
  | Productive (terrain, token, robberOpt) ->
      Printf.sprintf
        "<Productive : (terrain : %s, circularToken : %s , robber : %s)>"
        (terrain_to_string terrain)
        (circularToken_to_string token)
        (robberOpt_to_string robberOpt)
  | Barren (terrain, robberOpt) ->
      Printf.sprintf "<Barren : (terrain : %s, robber : %s)>"
        (terrain_to_string terrain)
        (robberOpt_to_string robberOpt)

let seaTile_to_string (title : seaTile) =
  match title with
  | Harbor token ->
      let cardStr =
        match token.resourceToGive with
        | Some card -> resourceCard_to_string card
        | None -> "<None>"
      in
      Printf.sprintf "<Harbor : { giveAmount : %i; resourceToGive : %s }>"
        token.giveAmount cardStr
  | Water -> "<Water>"

let hexEdge_to_string (edge : hexEdge) =
  match edge with
  | Road color -> Printf.sprintf "<Road : %s>" (playerColor_to_string color)
  | Empty -> "<Empty>"

let hexCorner_to_string (corner : hexCorner) =
  match corner with
  | Settlement color ->
      Printf.sprintf "<Settlement : %s>" (playerColor_to_string color)
  | Empty -> "<Empty>"

let gameBoard_to_string (item : gameBoardItem) =
  match item with
  | Terrain t -> Printf.sprintf "<Terrain : %s>" (terrainTile_to_string t)
  | Sea s -> Printf.sprintf "<Sea : %s>" (seaTile_to_string s)
  | Edge e -> Printf.sprintf "<Edge : %s>" (hexEdge_to_string e)
  | Corner c -> Printf.sprintf "<Corner : %s>" (hexCorner_to_string c)

let print_gameBoard_item (item : gameBoardItem) =
  print_string (gameBoard_to_string item)

let gameBoard_point_to_string (point : gameBoardPoint) =
  let ID id, item = point.item in
  let itemStr = gameBoard_to_string item in
  Printf.sprintf "{ x : %f; y : %f; item : (id : %d, gameBoardItem : %s) }\n"
    point.x point.y id itemStr

let print_gameBoard_point (point : gameBoardPoint) =
  print_string (gameBoard_point_to_string point)

let gameBoard_to_string (board : gameBoardPoint list) =
  List.map gameBoard_point_to_string board |> String.concat "\r\n"

let print_gameBoard (board : gameBoardPoint list) =
  print_string (gameBoard_to_string board)
