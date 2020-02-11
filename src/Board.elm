module Board exposing (Board, clearPos, getPos, init, nextGamePhase, nextTurnPhase, setPos, validMove, viewBoard)

import Array exposing (Array)
import Element exposing (Element, column, el, height, padding, px, rgb255, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Tuple exposing (first, second)
import Types exposing (..)


boardSize =
    6


type alias Board =
    { n : Maybe Int
    , s : Maybe Int
    , e : Maybe Int
    , w : Maybe Int
    , skippedMine : Bool
    , playArea : PlayArea
    }


init : Board
init =
    Board Nothing Nothing Nothing Nothing False <| Array.repeat (boardSize * boardSize) Empty


getPos : Board -> Position -> Maybe Mark
getPos board position =
    Array.get ((boardSize * (first position - 1)) + (second position - 1)) board.playArea


setPos : Board -> Position -> Mark -> Board
setPos board position mark =
    { board | playArea = Array.set ((boardSize * (first position - 1)) + (second position - 1)) mark board.playArea }


clearPos : PlayArea -> Position -> PlayArea
clearPos playArea position =
    Array.set ((boardSize * first position) + second position) Empty playArea


viewBoard : Board -> Element Msg
viewBoard board =
    let
        width =
            40
    in
    List.range 0 boardSize
        |> List.concatMap
            (\n ->
                [ Svg.line [ x1 "0", y1 (String.fromInt (n * width)), x2 (String.fromInt (boardSize * width)), y2 (String.fromInt (n * width)), stroke "black" ] []
                , Svg.line [ y1 "0", x1 (String.fromInt (n * width)), y2 (String.fromInt (boardSize * width)), x2 (String.fromInt (n * width)), stroke "black" ] []
                , Svg.rect [ onClick (GotBoardClick ( n, n )), x (String.fromInt (n * width)), y (String.fromInt (n * width)), Svg.Attributes.width (String.fromInt width), Svg.Attributes.height (String.fromInt width) ] []
                ]
            )
        |> svg
            [ Svg.Attributes.width "300"
            , Svg.Attributes.height "300"
            , viewBox "-5 -5 310 310"
            , strokeWidth "2"
            ]
        |> Element.html



--viewBoard : Board -> Element Msg
--viewBoard board =
--    List.range 1 boardSize
--        |> List.map (\r -> el [ Font.size 50 ] <| viewRow r board)
--        |> column []
--
--
--viewRow : Int -> Board -> Element Msg
--viewRow r board =
--    List.range 1 boardSize
--        |> List.map (\c -> viewSpace board ( r, c ))
--        |> row []
--
--
--viewSpace : Board -> Position -> Element Msg
--viewSpace board position =
--    let
--        r =
--            first position
--
--        c =
--            second position
--
--        v =
--            getPos board position
--    in
--    button [ width <| px 60, height <| px 60, Border.color <| rgb255 0 0 0, Border.width 2, padding 5 ] <|
--        case v of
--            --Just (Track n) ->
--            --    { onPress = Just (GotBoardClick position), label = text (String.fromInt n) }
--            Just Mountain ->
--                { onPress = Nothing, label = text "Λ" }
--
--            Just Mine ->
--                { onPress = Nothing, label = text "M" }
--
--            _ ->
--                { onPress = Just (GotBoardClick position), label = text "" }


type alias PlayArea =
    Array Mark


nextGamePhase : GamePhase -> GamePhase
nextGamePhase gamePhase =
    case gamePhase of
        PlaceMountains n ->
            if n < 6 then
                PlaceMountains (n + 1)

            else
                PlaceMine

        PlaceMine ->
            PlaceStations 1

        _ ->
            PlaceMine


nextTurnPhase : GamePhase -> TurnPhase -> TurnPhase
nextTurnPhase gamePhase turnPhase =
    case ( gamePhase, turnPhase ) of
        ( PlaceMountains n, _ ) ->
            if n < 6 then
                Roll

            else
                Place 1

        ( PlaceMine, _ ) ->
            Roll

        _ ->
            Roll


validMove : Position -> Mark -> GamePhase -> Board -> Int -> Bool
validMove position mark phase board roll =
    case ( phase, mark, position ) of
        ( New, _, _ ) ->
            False

        ( PlaceMountains row, Mountain, ( r, c ) ) ->
            (r == row)
                && (c == roll)
                && (List.range 1 boardSize
                        |> List.filterMap (\col -> getPos board ( row, col ))
                        |> List.filter (\x -> x /= Empty)
                        |> List.length
                        |> (==) 0
                   )

        ( PlaceMine, Mine, ( r, c ) ) ->
            (getPos board ( r, c ) == Just Empty)
                && ([ ( r + 1, c ), ( r - 1, c ), ( r, c + 1 ), ( r, c - 1 ) ]
                        |> List.filterMap (getPos board)
                        |> List.member Mountain
                   )

        --( ( Main, _ ), Track i, _ ) ->
        --    True
        ( _, _, _ ) ->
            False
