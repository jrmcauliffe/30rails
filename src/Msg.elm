module Msg exposing (GamePhase(..), GameState, Mark(..), Msg(..), Position, TurnPhase(..), getMark)


type alias Position =
    ( Int, Int )


type Mark
    = Mountain
    | Mine
    | Empty


getMark : GamePhase -> Mark
getMark phase =
    case phase of
        PlaceMountains n ->
            Mountain

        PlaceMine ->
            Mine

        _ ->
            Empty


type alias GameState =
    ( GamePhase, TurnPhase )


type GamePhase
    = New
    | PlaceMountains Int
    | PlaceMine
    | PlaceStations Int
    | PlaceBonus
    | Main
    | Gameover
    | Error String


type TurnPhase
    = Roll
    | Place Int


type Msg
    = ClickedRoll
    | ClickedStart
    | GotDiceIndex Int
    | GotBoardClick Position
