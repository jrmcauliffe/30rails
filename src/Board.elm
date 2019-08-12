module Board exposing (Position, PostitionStatus(..))


type alias Position =
    ( Int, Int )


type PostitionStatus
    = Empty
    | Mine
