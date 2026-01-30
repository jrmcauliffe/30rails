module Dice exposing
    ( Color(..)
    , DiceColors
    , DiceConfig
    , black
    , defaultConfig
    , dice
    , invertColors
    , view
    , white
    )

import Svg exposing (..)
import Svg.Attributes exposing (..)



-- Type-safe color representation


type Color
    = Hex String
    | Rgb Int Int Int
    | Named String


colorToString : Color -> String
colorToString color =
    case color of
        Hex s ->
            s

        Rgb r g b ->
            "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

        Named s ->
            s



-- Common colors


black : Color
black =
    Hex "#000"


white : Color
white =
    Hex "#fff"



-- Type-safe dice values (compile-time validation)


type DiceValue
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


fromInt : Int -> DiceValue
fromInt n =
    case n of
        1 ->
            One

        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        _ ->
            One


type alias DiceColors =
    { background : Color
    , pips : Color
    , border : Maybe Color
    }


type alias DiceConfig =
    { size : Float
    , colors : DiceColors
    }


defaultConfig : DiceConfig
defaultConfig =
    { size = 80
    , colors = { background = black, pips = white, border = Nothing }
    }



-- Pip positions (normalized 0-1)


type alias PipPosition =
    { x : Float, y : Float }


topLeft : PipPosition
topLeft =
    { x = 0.24, y = 0.24 }


topRight : PipPosition
topRight =
    { x = 0.76, y = 0.24 }


midLeft : PipPosition
midLeft =
    { x = 0.24, y = 0.5 }


center : PipPosition
center =
    { x = 0.5, y = 0.5 }


midRight : PipPosition
midRight =
    { x = 0.76, y = 0.5 }


bottomLeft : PipPosition
bottomLeft =
    { x = 0.24, y = 0.76 }


bottomRight : PipPosition
bottomRight =
    { x = 0.76, y = 0.76 }


pipPositions : DiceValue -> List PipPosition
pipPositions value =
    case value of
        One ->
            [ center ]

        Two ->
            [ topRight, bottomLeft ]

        Three ->
            [ topRight, center, bottomLeft ]

        Four ->
            [ topLeft, topRight, bottomLeft, bottomRight ]

        Five ->
            [ topLeft, topRight, center, bottomLeft, bottomRight ]

        Six ->
            [ topLeft, topRight, midLeft, midRight, bottomLeft, bottomRight ]



-- Core rendering


diceWithConfig : DiceConfig -> DiceValue -> Svg msg
diceWithConfig config value =
    Svg.svg
        [ Svg.Attributes.width (String.fromFloat config.size)
        , Svg.Attributes.height (String.fromFloat config.size)
        , viewBox "0 0 100 100"
        ]
        (renderBackground config :: List.map (renderPip config) (pipPositions value))


renderBackground : DiceConfig -> Svg msg
renderBackground config =
    let
        borderAttrs =
            case config.colors.border of
                Just color ->
                    [ stroke (colorToString color), strokeWidth "3" ]

                Nothing ->
                    []
    in
    Svg.rect
        ([ x "5"
         , y "5"
         , Svg.Attributes.width "90"
         , Svg.Attributes.height "90"
         , rx "15"
         , ry "15"
         , fill (colorToString config.colors.background)
         ]
            ++ borderAttrs
        )
        []


renderPip : DiceConfig -> PipPosition -> Svg msg
renderPip config pos =
    Svg.circle
        [ cx (String.fromFloat (pos.x * 100))
        , cy (String.fromFloat (pos.y * 100))
        , r "10"
        , fill (colorToString config.colors.pips)
        ]
        []



-- Inversion helper


invertColors : DiceColors -> DiceColors
invertColors c =
    { background = c.pips, pips = c.background, border = c.border }



-- Backward-compatible wrapper (keeps existing API working)


dice : Int -> Svg msg
dice n =
    diceWithConfig defaultConfig (fromInt n)



-- Configurable API


view : DiceConfig -> Int -> Svg msg
view config n =
    diceWithConfig config (fromInt n)
