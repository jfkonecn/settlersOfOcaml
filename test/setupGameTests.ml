open OUnit2
open SettlersOfOcaml
open SettlersOfOcaml.Types

let startGame = SettlersOfOcaml.startGame


let tooFewPlayersShouldCreateError _ =
    let _ = match startGame [] with
    | Error err -> (List.find (fun x -> NotEnoughPlayers = x) err) 
    | _ -> assert_failure ("Expected an Error") in
    assert_bool "" true

let tooManyCharactersShouldGenerateAnError _ =
    let badPlayer:playerBlueprint = (Name (List.init 100 (fun _ -> "a") |> String.concat ""), Red) in
    match startGame [badPlayer] with
    | Error err -> 
        let _ = List.find (fun x -> NameExceededCharacterLimit (50, badPlayer) = x) err in
        ()
    | _ -> assert_failure ("Expected an Error")
    
let duplicateColorsShouldGenerateAnError _ =
    let badPlayers:playerBlueprint list = [(Name "a", Red);(Name "a", Red)] in

    let testPlayer x y =
        let (Name expectedName, expectedColor) = x in
        let (Name actualName, actualColor) = y in
        assert_equal expectedName actualName;
        assert_equal expectedColor actualColor in

    let errorNotUnderTest x = 
        match x with
        | DuplicatedColor (Red, players) -> List.map2 testPlayer badPlayers players |> fun _ -> true
        | _ -> false in

    let testError x =
        x 
        |> List.filter errorNotUnderTest 
        |> fun x -> assert_equal 1 (List.length x) in

    match startGame badPlayers with
    | Error x -> testError x
    | _ -> assert_failure ("Expected an Error")

let shouldCreateGame _ =
    let players:playerBlueprint list = [(Name "red", Red);(Name "blue", Blue)] in
    let testPlayer x y =
        let (Name expectedName, expectedColor) = x in
        assert_equal expectedName y.name;
        assert_equal expectedColor y.color;
        assert_equal {
            totalSettlements=5;
            totalCities=4;
            totalRoads=15;
        } y.hand in

    let testPlayers x =
        x 
        |> List.map2 testPlayer players 
        |> fun _ -> () in

    let testBoard (board:gameBoardPoint array) =
        let boardPoints = board |> Array.map(fun x -> x.x, x.y) in
        assert_equal boardPoints boardPoints in

    match startGame players with
    | Ok x -> testPlayers x.players |> (fun () -> testBoard x.gameBoard)
    | _ -> assert_failure ("Expected no Errors")


let robberShouldBePlacedInTheDesert _ =
    let players:playerBlueprint list = [(Name "red", Red);(Name "blue", Blue)]
    and hasRobber (p:gameBoardPoint) =
        match p.item with
        | Terrain Productive (_, _, Some ()) -> true
        | Terrain Barren (_, Some ()) -> true
        | _ -> false
    and tapAssertFunReturnsTrue f x =
        assert_equal (f x) true;
        x in
    let f (g:game) =
        g.gameBoard
        |> Array.to_list
        |> List.filter hasRobber
        |> tapAssertFunReturnsTrue (fun x -> List.length x = 1) 
        |> List.hd
        |> tapAssertFunReturnsTrue (fun x -> x.item = Terrain (Barren (Desert, Some ())))
        |> (fun _ -> ()) in
    match startGame players with
    | Ok x -> f x
    | _ -> assert_failure ("Expected no Errors")

let tests = "test suite for game setup" >::: [
  "Too Few Players Should Create an Error"  >:: tooFewPlayersShouldCreateError;
  "Too Many Characters Should Generate an Error"  >:: tooManyCharactersShouldGenerateAnError;
  "Duplicate Colors Should Generate an Error"  >:: duplicateColorsShouldGenerateAnError;
  "Should Create Game"  >:: shouldCreateGame;
  "Robber Should Be Placed In The Desert"  >:: robberShouldBePlacedInTheDesert;
]