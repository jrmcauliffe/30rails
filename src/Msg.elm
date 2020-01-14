module Msg exposing (Msg(..), Phase(..), Position, Roll)


type alias Position =
    ( Int, Int )


type alias Roll =
    Int


type Phase
    = New
    | PlaceMountains Int
    | PlaceMine
    | PlaceStations
    | PlaceBonus
    | Main
    | Gameover
    | Error


type Msg
    = ClickedRoll
    | ClickedStart
    | GotDiceIndex Roll
    | GotBoardClick Position
