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

type hexCorner = House of playerColor | Empty

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
