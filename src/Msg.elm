module Msg exposing (Msg(..), Phase(..), Position)


type alias Position =
    ( Int, Int )


type Phase
    = New
    | PlaceMountains
    | PlaceMine
    | PlaceStations
    | PlaceBonus
    | Main
    | Gameover
    | Error


type Msg
    = ClickedRoll
    | ClickedStart
    | GotDiceIndex Int
    | GotBoardClick Position
