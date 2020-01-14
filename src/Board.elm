module Board exposing (Board, Mark(..), clearPos, getPos, init, setPos, viewBoard)

import Array exposing (Array)
import Element exposing (Element, column, el, height, padding, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Msg exposing (Msg(..), Phase(..), Position, Roll)
import Tuple exposing (first, second)


type Mark
    = Track Int
    | Mountain
    | Mine
    | Empty


boardSize =
    6


init : Board
init =
    Board Nothing Nothing Nothing Nothing Nothing False <| Array.repeat (boardSize * boardSize) Empty


getPos : Board -> Position -> Maybe Mark
getPos board position =
    Array.get ((boardSize * first position) + second position) board.playArea


setPos : Board -> Phase -> Position -> Roll -> Mark -> Board
setPos board phase position roll mark =
    if validMove position mark phase roll board then
        { board | playArea = Array.set ((boardSize * first position) + second position) Mountain board.playArea }

    else
        board


clearPos : PlayArea -> Position -> PlayArea
clearPos playArea position =
    Array.set ((boardSize * first position) + second position) Empty playArea


viewBoard : Board -> Element Msg
viewBoard board =
    List.range 1 boardSize
        |> List.map (\r -> el [ Font.size 50 ] <| viewRow r board)
        |> column []


viewRow : Int -> Board -> Element Msg
viewRow r board =
    List.range 1 boardSize
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

        --w =
        --    { bottom =
        --        if r > boardSize then
        --            0
        --
        --        else
        --            2
        --    , left =
        --        if c == 0 then
        --            0
        --
        --        else
        --            2
        --    , right =
        --        if c > boardSize then
        --            0
        --
        --        else
        --            2
        --    , top =
        --        if r == 0 then
        --            0
        --
        --        else
        --            2
        --    }
    in
    button [ width <| px 60, height <| px 60, Border.color <| rgb255 0 0 0, Border.width 2, padding 5 ] <|
        case v of
            Just (Track n) ->
                { onPress = Just (GotBoardClick position), label = text (String.fromInt n) }

            Just Mountain ->
                { onPress = Just (GotBoardClick position), label = text "Λ" }

            Just Mine ->
                { onPress = Just (GotBoardClick position), label = text "M" }

            _ ->
                { onPress = Just (GotBoardClick position), label = text "" }


type alias Board =
    { n : Maybe Int
    , s : Maybe Int
    , e : Maybe Int
    , w : Maybe Int
    , sr : Maybe Int
    , skippedMine : Bool
    , playArea : PlayArea
    }


type alias PlayArea =
    Array Mark


validMove : Position -> Mark -> Phase -> Roll -> Board -> Bool
validMove position mark phase roll board =
    case ( phase, mark, position ) of
        ( New, _, _ ) ->
            False

        ( PlaceMountains row, Mountain, ( r, _ ) ) ->
            (r == row) && (getPos board position == Just Empty)

        ( PlaceMine, Mine, _ ) ->
            True

        ( Main, Track i, _ ) ->
            True

        ( _, _, _ ) ->
            False
