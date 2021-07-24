open Types

exception TokenException of string

let validateBlueprints playerBlueprints =
  let notEnoughPlayers =
    if List.length playerBlueprints < 2 then [ NotEnoughPlayers ] else []
  and characterLimit = 50 in
  let tooManyCharacters =
    playerBlueprints
    |> List.filter (fun (Name name, _) -> String.length name > characterLimit)
    |> List.map (fun x -> NameExceededCharacterLimit (characterLimit, x))
  in

  let duplicatedColor =
    playerBlueprints
    |> Linq.groupBy (fun (_, color) -> color)
    |> List.filter (fun (_, x) -> List.length x <> 1)
    |> List.map (fun (x, y) -> DuplicatedColor (x, y))
  in

  let errors =
    notEnoughPlayers
    |> List.append tooManyCharacters
    |> List.append duplicatedColor
  in

  if errors = [] then Ok playerBlueprints else Error errors

let startWithSeedGame seed playerBlueprints =
  Random.init seed;
  let createPlayer (Name name, color) =
    {
      name;
      color;
      hand = { totalSettlements = 5; totalCities = 4; totalRoads = 15 };
    }
  in
  let shuffle x = Linq.sortByInt (fun _ -> Random.int 1000) x in

  let board =
    let shuffledNumberTokens =
      BoardPieces.numberTokens |> Linq.sortByChar (fun x -> x.letter)
    in

    let rec buildTerrainTiles numTokens terrains =
      let createProductiveTile terrian =
        match numTokens with
        | head :: tail -> (Terrain (Productive (terrian, head, None)), tail)
        | [] -> raise (TokenException "Ran out of number tokens")
      in
      let createTerrainTile terrian =
        match terrian with
        | Desert -> (Terrain (Barren (terrian, Some Robber)), numTokens)
        | _ -> createProductiveTile terrian
      in
      match terrains with
      | terrainHead :: terrainTail ->
          let terrainTile, remainingTokens = createTerrainTile terrainHead in
          terrainTile :: buildTerrainTiles remainingTokens terrainTail
      | [] -> []
    in

    let terrainTiles =
      BoardPieces.terrainHexes |> shuffle
      |> buildTerrainTiles shuffledNumberTokens
    in

    let rec buildSeaTiles harborTokens terrains =
      let createSeaTiles x =
        match (x, harborTokens) with
        | SingleHarbor, h1 :: tail -> ([ Water; Harbor h1; Water ], tail)
        | TwoHarbor, h1 :: h2 :: tail -> ([ Harbor h1; Water; Harbor h2 ], tail)
        | _ -> raise (TokenException "Ran out of harbor tokens")
      in
      match terrains with
      | seaHead :: seaTail ->
          let seaTiles, remainingTokens = createSeaTiles seaHead in
          seaTiles @ buildSeaTiles remainingTokens seaTail
      | [] -> []
    in
    let shuffledHarborTokens = BoardPieces.harborTokens |> shuffle in

    (* TODO: find a better way to round... *)
    let round2 n = Float.round (n *. 100.) /. 100. in

    let seaTiles =
      BoardPieces.seaFrames |> shuffle
      |> buildSeaTiles shuffledHarborTokens
      |> List.map (fun x -> Sea x)
    and createAllPolarPoints (x, y) =
      let theta = Float.atan (y /. x)
      and r = Float.sqrt ((x *. x) +. (y *. y)) in
      List.init 5 (fun x -> float_of_int x)
      |> List.map (fun x -> x *. 60.0 *. Float.pi /. 180.0)
      |> List.map (fun x -> (r, x +. theta))
    and polarToCartesian x =
      x
      |> List.map (fun (r, t) -> (r *. Float.cos t, r *. Float.sin t))
      |> List.map (fun (r, t) -> (round2 r, round2 t))
    in

    let createAllCartesianPoints (x, y) =
      createAllPolarPoints (x, y) |> polarToCartesian
    and hexCenterToEdge = 1.0 in
    let hexEdgeLength = 2.0 /. 3.0 *. Float.sqrt 3.0 *. hexCenterToEdge in

    let assignIds startingId items =
      let itemIds =
        List.init (items |> List.length) (fun x -> x + startingId)
      in
      Linq.zip itemIds items
    in

    let hexPoints =
      let f x =
        x
        |> List.map createAllPolarPoints
        |> List.concat
        |> Linq.sortByFloat (fun (_, t) -> t)
        |> polarToCartesian
      in
      let seaPoints =
        f
          [
            (hexCenterToEdge *. 6.0, 0.0);
            (hexCenterToEdge *. 5.0, hexEdgeLength *. 1.5);
            (hexCenterToEdge *. 4.0, hexEdgeLength *. 3.0);
          ]
      and level3Terrain =
        f
          [
            (hexCenterToEdge *. 4.0, 0.0);
            (hexCenterToEdge *. 3.0, hexEdgeLength *. 1.5);
          ]
      and level2Terrain = f [ (hexCenterToEdge *. 2.0, 0.0) ] in
      seaPoints @ level3Terrain @ level2Terrain @ [ (0.0, 0.0) ]
      |> Linq.zip (seaTiles @ terrainTiles)
      |> assignIds 1
      |> List.map (fun (id, (tile, (x, y))) -> { x; y; item = (ID id, tile) })
    in

    let cornerPoints =
      [
        (hexCenterToEdge *. 5.0, hexEdgeLength *. 0.5);
        (hexCenterToEdge *. 3.0, hexEdgeLength *. 3.5);
        (hexCenterToEdge *. 4.0, hexEdgeLength *. 2.0);
        (hexCenterToEdge *. 4.0, hexEdgeLength *. 1.0);
        (hexCenterToEdge *. 3.0, hexEdgeLength *. 2.5);
        (hexCenterToEdge *. 3.0, hexEdgeLength *. 0.5);
        (hexCenterToEdge *. 2.0, hexEdgeLength *. 2.0);
        (hexCenterToEdge *. 2.0, hexEdgeLength *. 1.0);
        (hexCenterToEdge *. 1.0, hexEdgeLength *. 0.5);
      ]
      |> List.map createAllCartesianPoints
      |> List.concat
      |> assignIds (List.length hexPoints + 1)
      |> List.map (fun (id, (x, y)) -> { x; y; item = (ID id, Corner Empty) })
    in

    let edgePoints =
      [
        (hexCenterToEdge *. 5.0, hexEdgeLength *. 0.0);
        (hexCenterToEdge *. 4.5, hexEdgeLength *. (Float.sqrt 3.0 /. 2.0));
        (hexCenterToEdge *. 3.0, hexEdgeLength *. 3.0);
        (hexCenterToEdge *. 4.0, hexEdgeLength *. 1.5);
        ( hexCenterToEdge *. 3.5,
          (hexEdgeLength *. (Float.sqrt 3.0 /. 2.0)) +. (hexCenterToEdge *. 1.5)
        );
        (hexCenterToEdge *. 3.5, hexEdgeLength *. (Float.sqrt 3.0 /. 2.0));
        ( hexCenterToEdge *. 2.5,
          (hexEdgeLength *. (Float.sqrt 3.0 /. 2.0)) +. (hexCenterToEdge *. 1.5)
        );
        (hexCenterToEdge *. 3.0, hexEdgeLength *. 0.0);
        (hexCenterToEdge *. 2.5, hexEdgeLength *. (Float.sqrt 3.0 /. 2.0));
        (hexCenterToEdge *. 2.0, hexEdgeLength *. 1.5);
        (hexCenterToEdge *. 1.5, hexEdgeLength *. (Float.sqrt 3.0 /. 2.0));
        (hexCenterToEdge *. 1.0, hexEdgeLength *. 0.0);
      ]
      |> List.map createAllCartesianPoints
      |> List.concat
      |> assignIds (List.length hexPoints + List.length cornerPoints + 1)
      |> List.map (fun (id, (x, y)) -> { x; y; item = (ID id, Edge Empty) })
    in

    hexPoints @ cornerPoints @ edgePoints
    |> Linq.sortBy2TupleFloat (fun x -> (-1.0 *. x.y, x.x))
  in

  (*
     https://www.redblobgames.com/grids/hexagons/
     Axial coordinates
  *)
  let startingPlayer : playerColor =
    match Random.int 4 with 0 -> Red | 1 -> White | 2 -> Orange | _ -> Blue
  in

  let createGame playerBlueprints =
    {
      players = playerBlueprints |> List.map createPlayer;
      gameBoard = board;
      availableResourceCards =
        { brick = 19; grain = 19; lumber = 19; ore = 19; wool = 19 };
      round = 1;
      developmentCards = BoardPieces.developmentCards |> shuffle;
      startingColor = startingPlayer;
      currentColor = startingPlayer;
    }
  in

  validateBlueprints playerBlueprints |> Result.map createGame
