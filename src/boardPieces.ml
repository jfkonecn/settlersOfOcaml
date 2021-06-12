(* https://catan.fandom.com/wiki/Catan *)
open Types
    let seaFrames = 
        List.init 3 (fun _ -> SingleHarbor) 
        @ List.init 3 (fun _ -> TwoHarbor) 

    let harborTokens = 
        ([Lumber;Wool;Grain;Brick;Ore] |> List.map (fun x -> {  giveAmount=2; resourceToGive=Some x })) 
        @ List.init 4 (fun _ -> {  giveAmount=3; resourceToGive=None; })

    let terrainHexes = 
        List.init 4 (fun _ -> Forest) 
        @ List.init 4 (fun _ -> Pasture)
        @ List.init 4 (fun _ -> Fields)
        @ List.init 3 (fun _ -> Hills)
        @ List.init 3 (fun _ -> Mountains)
        @ [ Desert ] 

    let resourceCards = 
      List.init 19 (fun _ -> Lumber)
        @ List.init 19 (fun _ -> Wool)
        @ List.init 19 (fun _ -> Grain)
        @ List.init 19 (fun _ -> Brick)
        @ List.init 19 (fun _ -> Ore)

    let numberTokens =
        [
            { letter='B'; value=2; color=Black; };
            { letter='C'; value=6; color=Red; };
            { letter='P'; value=6; color=Red; };
            { letter='E'; value=8; color=Red; };
            { letter='K'; value=8; color=Red; };
            { letter='H'; value=12; color=Black; };
            { letter='D'; value=3; color=Black; };
            { letter='Q'; value=3; color=Black; };
            { letter='J'; value=4; color=Black; };
            { letter='N'; value=4; color=Black; };
            { letter='A'; value=5; color=Black; };
            { letter='O'; value=5; color=Black; };
            { letter='G'; value=9; color=Black; };
            { letter='M'; value=9; color=Black; };
            { letter='L'; value=10; color=Black; };
            { letter='F'; value=10; color=Black; };
            { letter='I'; value=11; color=Black; };
            { letter='R'; value=11; color=Black; };
        ]

