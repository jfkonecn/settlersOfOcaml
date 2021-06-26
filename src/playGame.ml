open Types

let getAvailableMoves (_ : game) = [ PlaceSettlement ]

let makeMove move game =
  let isMatch x y = x = y in

  let continueWithMove () = Ok game in

  let moveIfValid allowedMoves =
    match List.find_opt (isMatch move) allowedMoves with
    | Some _ -> continueWithMove ()
    | None -> Error (InvalidMove move)
  in

  getAvailableMoves game |> moveIfValid
