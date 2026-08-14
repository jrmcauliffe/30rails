module Tiles exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Tuple exposing (first, second)
import Types exposing (..)


drawTile : Position -> Float -> Mark -> Svg Msg
drawTile pos tileWidth mark =
    Svg.rect
        [ onClick (GotBoardClick ( first pos, second pos ))
        , x (String.fromFloat (toFloat (first pos - 1) * tileWidth))
        , y (String.fromFloat (toFloat (second pos - 1) * tileWidth))
        , Svg.Attributes.width (String.fromFloat tileWidth)
        , Svg.Attributes.height (String.fromFloat tileWidth)
        , fill "rgb(12,128,128)"
        ]
        []
