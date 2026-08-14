module Layout exposing (Metrics, fromViewport, page, panel, stack, boardWidth, lineSpacing)

{-| Everything that depends on the size of the browser window: how big the board
should be, how big the type should be, and which way round the page is arranged.

Callers ask for a `Metrics` once per render and read sizes off it. The
orientation itself is deliberately not exposed — `page`, `panel` and `stack`
already make every arrangement decision that depends on it, so views never need
to branch on the window shape themselves.

@docs Metrics, fromViewport, page, panel, stack, boardWidth, lineSpacing

-}

import Element exposing (Attribute, Element, alignTop, centerX, column, fill, height, padding, px, row, spacing, width)
import Html exposing (Html)
import Types exposing (Viewport)


{-| Which way round the page is arranged. Narrow or tall windows stack the panel
under the board; wide ones put it alongside.
-}
type Orientation
    = Portrait
    | Landscape


{-| The derived sizes for one render, in CSS pixels.
-}
type alias Metrics =
    { orientation : Orientation
    , board : Int
    , pad : Int
    , spacing : Int
    , titleFont : Int
    , bodyFont : Int
    , buttonFont : Int
    , panelDice : Float
    }


fromViewport : Viewport -> Metrics
fromViewport viewport =
    let
        orientation =
            if viewport.width >= 700 && viewport.width > viewport.height then
                Landscape

            else
                Portrait

        {- Scale factor for padding and spacing, taken from the window's short
           edge against an 800px reference.
        -}
        k =
            clamp 0.5 1.0 (toFloat (min viewport.width viewport.height) / 800)

        pad =
            clamp 10 30 (k * 30)

        {- Size the board out of what's left once padding is taken, so it can
           never overflow, and cap the long edge to leave room for the title,
           hint and panel.
        -}
        availWidth =
            toFloat viewport.width - (pad * 2)

        availHeight =
            toFloat viewport.height - (pad * 2)

        board =
            case orientation of
                Landscape ->
                    min (availHeight * 0.78) (availWidth * 0.62)

                Portrait ->
                    min availWidth (availHeight * 0.62)
    in
    { orientation = orientation
    , board = round board
    , pad = round pad
    , spacing = round (clamp 8 24 (k * 24))

    {- Type is measured against the board rather than the window, so the labels
       grow and shrink in step with the thing they sit under. The divisors are
       set so a 600px board reproduces the original fixed sizes; the clamps keep
       text readable on a phone and stop it ballooning on a monitor.
    -}
    , titleFont = round (clamp 26 56 (board / 12))
    , bodyFont = round (clamp 13 22 (board / 40))
    , buttonFont = round (clamp 18 34 (board / 20))
    , panelDice = clamp 36 72 (board / 11)
    }



-- Arrangement


{-| The whole page: board first, then the panel beside it or below it.
-}
page : Metrics -> List (Element msg) -> Html msg
page m children =
    Element.layout [ width fill, height fill ] <|
        alongside m
            [ width fill, height fill, padding m.pad, spacing m.spacing, centerX ]
            children


{-| The controls panel, which runs down the side of a wide window and across the
bottom of a narrow one.
-}
panel : Metrics -> List (Element msg) -> Element msg
panel m children =
    across m
        [ padding m.pad, spacing m.spacing, centerX, alignTop ]
        children


{-| The board and the labels that belong under it, as one centred column.
-}
stack : Metrics -> List (Element msg) -> Element msg
stack m children =
    column [ centerX, alignTop, spacing m.spacing ] children


{-| Bound an element to the width of the board, so labels line up underneath it
and long text wraps rather than running off the side of the window.
-}
boardWidth : Metrics -> Attribute msg
boardWidth m =
    width (px m.board)


{-| Leading for wrapped body text.
-}
lineSpacing : Metrics -> Attribute msg
lineSpacing m =
    spacing (m.bodyFont // 3)



-- Internals


{-| Lays children out along the window's long edge.
-}
alongside : Metrics -> (List (Attribute msg) -> List (Element msg) -> Element msg)
alongside m =
    case m.orientation of
        Landscape ->
            row

        Portrait ->
            column


{-| Lays children out across the window's long edge, i.e. the other way to
`alongside`.
-}
across : Metrics -> (List (Attribute msg) -> List (Element msg) -> Element msg)
across m =
    case m.orientation of
        Landscape ->
            column

        Portrait ->
            row
