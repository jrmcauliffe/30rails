module Board exposing (Position, PostitionStatus(..))

import Dict exposing (Dict)


type alias Position =
    ( Int, Int )


type PostitionStatus
    = Empty
    | Mine


type alias Board =
    Dict Position PostitionStatus
