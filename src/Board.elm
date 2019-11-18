module Board exposing (Position, PostitionStatus(..))

import Dict exposing (Dict)


type alias Position =
    ( Int, Int )


type GamePhase
    = NewGame
    | SetupPhase
    | MainPhase
    | GameOver


type PostitionStatus
    = Empty
    | Mine


type alias Board =
    Dict Position PostitionStatus
