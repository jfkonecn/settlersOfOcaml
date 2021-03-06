type id = ID of int

type playerColor = Red | White | Orange | Blue

type hand = { totalSettlements : int; totalCities : int; totalRoads : int }

type playerName = Name of string

type playerBlueprint = playerName * playerColor

type player = { name : string; color : playerColor; hand : hand }

type resourceCard = Brick | Grain | Lumber | Ore | Wool

type terrain = Hills | Forest | Mountains | Fields | Pasture | Desert

type seaFrame = SingleHarbor | TwoHarbor

type harborToken = { giveAmount : int; resourceToGive : resourceCard option }

type circularTokenColor = Black | Red

type circularToken = { value : int; color : circularTokenColor; letter : char }

type specialCard = LongestRoad | LargestArmy

type city = { color : playerColor }

type settlement = { color : playerColor }

type robber = Robber

type terrainTile =
  | Productive of terrain * circularToken * robber option
  | Barren of terrain * robber option

type developmentCard = KnightCard | ProgressCard | VictoryPointCard

type seaTile = Harbor of harborToken | Water

type hexEdge = Road of playerColor | Empty

type hexCorner = Settlement of playerColor | Empty

type gameBoardItem =
  | Terrain of terrainTile
  | Sea of seaTile
  | Edge of hexEdge
  | Corner of hexCorner

type gameBoardPoint = { x : float; y : float; item : id * gameBoardItem }

type availableResourceCards = {
  brick : int;
  grain : int;
  lumber : int;
  ore : int;
  wool : int;
}

type game = {
  gameBoard : gameBoardPoint list;
  players : player list;
  availableResourceCards : availableResourceCards;
  developmentCards : developmentCard list;
  round : int;
  startingColor : playerColor;
  currentColor : playerColor;
}

type move = PlaceSettlement | PlaceRoad | EndTurn

type gameError =
  | NameExceededCharacterLimit of int * playerBlueprint
  | DuplicatedColor of playerColor * playerBlueprint list
  | NotEnoughPlayers
  | InvalidMove of move
  | BoardItemNotFound of id
  | ItemIsNotACorner of id

module Stateful = struct
  type ('a, 's) stateful = Stateful of ('s -> 'a * 's)

  let run state (Stateful f) = f state

  let bind f m =
    Stateful
      (fun s ->
        let a', s' = run s m in
        f a' |> run s')

  let return a = Stateful (fun s -> (a, s))

  let returnFun f = Stateful (fun s -> f s)

  let map f m = bind (fun x -> return (f x)) m

  let mapWithState f m =
    Stateful
      (fun s ->
        let a', s' = run s m in
        f (a', s'))

  let join m = bind (fun x -> x) m

  let peek (f : 's -> 'a) = Stateful (fun s -> (f s, s))

  let bindPeek f = bind (fun _ -> peek f)
end
