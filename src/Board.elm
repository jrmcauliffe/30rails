module Board exposing (Board, Mark(..), clearPos, getPos, init, setPos, viewBoard)

import Dict exposing (Dict)
import Element exposing (Element, column, el, height, padding, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Msg exposing (Msg(..), Position)
import Tuple exposing (first, second)


type Mark
    = Track Int
    | Mountain
    | Mine


init : Board
init =
    Board Nothing Nothing Nothing Nothing Nothing Dict.empty


getPos : Board -> Position -> Maybe Mark
getPos board position =
    Dict.get position board.playArea


setPos : Board -> Position -> Mark -> Board
setPos board position mark =
    let
        newPlayArea =
            Dict.insert position mark board.playArea
    in
    { board | playArea = newPlayArea }


clearPos : Board -> Position -> Board
clearPos board position =
    let
        newPlayArea =
            Dict.remove position board.playArea
    in
    { board | playArea = newPlayArea }


viewBoard : Board -> Element Msg
viewBoard board =
    List.range 0 7
        |> List.map (\r -> el [ Font.size 50 ] <| viewRow r board)
        |> column []


viewRow : Int -> Board -> Element Msg
viewRow r board =
    List.range 0 7
        |> List.map (\c -> viewSpace board ( r, c ))
        |> row
            (if board.sr == Just r then
                [ Background.color (rgb255 200 200 200) ]

             else
                []
            )


viewSpace : Board -> Position -> Element Msg
viewSpace board position =
    let
        r =
            first position

        c =
            second position

        v =
            getPos board position

        w =
            { bottom =
                if r == 7 then
                    0

                else
                    2
            , left =
                if c == 0 then
                    0

                else
                    2
            , right =
                if c == 7 then
                    0

                else
                    2
            , top =
                if r == 0 then
                    0

                else
                    2
            }
    in
    button [ width <| px 60, height <| px 60, Border.color <| rgb255 0 0 0, Border.widthEach w, padding 5 ] <|
        case v of
            Just (Track n) ->
                { onPress = Just (GotBoardClick position), label = text (String.fromInt n) }

            Just Mountain ->
                { onPress = Just (GotBoardClick position), label = text "Λ" }

            Just Mine ->
                { onPress = Just (GotBoardClick position), label = text "M" }

            Nothing ->
                { onPress = Just (GotBoardClick position), label = text "" }


type alias Board =
    { n : Maybe Int
    , s : Maybe Int
    , e : Maybe Int
    , w : Maybe Int
    , sr : Maybe Int
    , playArea : Dict Position Mark
    }
