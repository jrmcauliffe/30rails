module Board exposing (Board, viewBoard)

import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, height, padding, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Msg exposing (Msg)


type alias Position =
    ( Int, Int )


type GamePhase
    = NewGame
    | SetupPhase
    | MainPhase
    | GameOver


viewBoard : Board -> Element Msg
viewBoard board =
    List.range 1 6
        |> List.map (\r -> el [] <| viewRow r board)
        |> column []


viewRow : Int -> Board -> Element Msg
viewRow r board =
    List.range 1 6
        |> List.map (\c -> viewSpace board ( r, c ))
        |> row
            (if board.sr == Just r then
                [ Background.color (rgb255 200 200 200) ]

             else
                []
            )


viewSpace : Board -> ( Int, Int ) -> Element Msg
viewSpace board ( r, c ) =
    let
        v =
            Dict.get ( r, c ) board.playArea

        w =
            { bottom =
                if r == 6 then
                    4

                else
                    2
            , left =
                if c == 1 then
                    4

                else
                    2
            , right =
                if c == 6 then
                    4

                else
                    2
            , top =
                if r == 1 then
                    4

                else
                    2
            }
    in
    el [ width <| px 60, height <| px 60, Border.color <| rgb255 0 0 0, Border.widthEach w, padding 5 ] <|
        case v of
            Just a ->
                text (String.fromInt a)

            Nothing ->
                text ""


viewFace model =
    text <| String.fromInt model.face


type alias Board =
    { n : Maybe Int
    , s : Maybe Int
    , e : Maybe Int
    , w : Maybe Int
    , sr : Maybe Int
    , playArea : Dict ( Int, Int ) Int
    }
