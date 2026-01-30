module Tiles exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Tuple exposing (first, second)
import Types exposing (..)


drawTile : Position -> Int -> Mark -> Svg Msg
drawTile pos tileWidth mark =
    Svg.rect
        [ onClick (GotBoardClick ( first pos, second pos ))
        , x (String.fromInt ((first pos - 1) * tileWidth))
        , y (String.fromInt ((second pos - 1) * tileWidth))
        , Svg.Attributes.width (String.fromInt tileWidth)
        , Svg.Attributes.height (String.fromInt tileWidth)
        , fill "rgb(12,128,128)"
        ]
        []
