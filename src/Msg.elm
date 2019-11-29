module Msg exposing (Msg(..), Position, Phase(..))


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
    | GotDiceIndex Int
    | GotBoardClick Position
