module Msg exposing (Msg(..), Phase(..), Position, State(..))


type alias Position =
    ( Int, Int )


type Phase
    = New
    | PlaceMountains Int
    | PlaceMine
    | PlaceStations Int
    | PlaceBonus
    | Main
    | Gameover
    | Error String


type State
    = Roll
    | Place Int


type Msg
    = ClickedRoll
    | ClickedStart
    | GotDiceIndex Int
    | GotBoardClick Position
