module Msg exposing (Msg(..), Position)


type alias Position =
    ( Int, Int )


type Msg
    = ClickedRoll
    | GotDiceIndex Int
    | GotBoardClick Position
