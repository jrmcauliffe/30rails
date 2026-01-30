module Board exposing (Board, clearPos, getPos, init, nextGamePhase, nextTurnPhase, renderBoard, setPos, validMove)

import Array exposing (Array)
import Dice exposing (Color(..), DiceConfig)
import Element exposing (Element, column, el, height, padding, px, rgb255, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tiles exposing (drawTile)
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


renderBoard : Board -> Element Msg
renderBoard board =
    let
        cellSize =
            60

        diceMargin =
            3

        diceSize =
            cellSize - (diceMargin * 2)

        diceSizeStr =
            String.fromInt diceSize

        diceConfig =
            { size = toFloat diceSize
            , colors = { background = Dice.white, pips = Dice.black, border = Just Dice.black }
            }

        positions =
            List.range 1 boardSize
                |> List.concatMap (\r -> List.range 1 boardSize |> List.map (\c -> ( r, c )))
                |> List.filterMap
                    (\pos -> getPos board pos |> Maybe.map (drawTile pos cellSize))

        bumpers =
            let
                drawbox : Position -> Position -> Svg Msg
                drawbox from to =
                    Svg.rect
                        [ x (String.fromInt (cellSize * first from))
                        , y (String.fromInt (cellSize * second from))
                        , Svg.Attributes.width (String.fromInt (cellSize * (first to - first from + 1)))
                        , Svg.Attributes.height (String.fromInt (cellSize * (second to - second from + 1)))
                        , fill "rgb(192,192,192)"
                        ]
                        []
            in
            [ drawbox ( 2, 1 ) ( 7, 1 ) -- N
            , drawbox ( 2, 8 ) ( 7, 8 ) -- S
            , drawbox ( 8, 2 ) ( 8, 7 ) -- E
            , drawbox ( 1, 2 ) ( 1, 7 ) -- W
            ]

        headerDice =
            List.range 1 6
                |> List.map
                    (\n ->
                        Svg.foreignObject
                            [ x (String.fromInt (cellSize * (n + 1) + diceMargin))
                            , y (String.fromInt diceMargin)
                            , Svg.Attributes.width diceSizeStr
                            , Svg.Attributes.height diceSizeStr
                            ]
                            [ Dice.view diceConfig n ]
                    )

        footerDice =
            List.range 1 6
                |> List.map
                    (\n ->
                        Svg.foreignObject
                            [ x (String.fromInt (cellSize * (n + 1) + diceMargin))
                            , y (String.fromInt (cellSize * 9 + diceMargin))
                            , Svg.Attributes.width diceSizeStr
                            , Svg.Attributes.height diceSizeStr
                            ]
                            [ Dice.view diceConfig n ]
                    )

        leftDice =
            List.range 1 6
                |> List.map
                    (\n ->
                        Svg.foreignObject
                            [ x (String.fromInt diceMargin)
                            , y (String.fromInt (cellSize * (n + 1) + diceMargin))
                            , Svg.Attributes.width diceSizeStr
                            , Svg.Attributes.height diceSizeStr
                            ]
                            [ Dice.view diceConfig n ]
                    )

        rightDice =
            List.range 1 6
                |> List.map
                    (\n ->
                        Svg.foreignObject
                            [ x (String.fromInt (cellSize * 9 + diceMargin))
                            , y (String.fromInt (cellSize * (n + 1) + diceMargin))
                            , Svg.Attributes.width diceSizeStr
                            , Svg.Attributes.height diceSizeStr
                            ]
                            [ Dice.view diceConfig n ]
                    )

        lines =
            List.range 0 boardSize
                |> List.concatMap
                    (\n ->
                        [ Svg.line [ x1 "0", y1 (String.fromInt ((n + 2) * cellSize)), x2 (String.fromInt ((boardSize + 4) * cellSize)), y2 (String.fromInt ((n + 2) * cellSize)), stroke "black" ] []
                        , Svg.line [ y1 "0", x1 (String.fromInt ((n + 2) * cellSize)), y2 (String.fromInt ((boardSize + 4) * cellSize)), x2 (String.fromInt ((n + 2) * cellSize)), stroke "black" ] []
                        ]
                    )

        totalSize =
            cellSize * 10

        totalSizeStr =
            String.fromInt totalSize

        viewBoxStr =
            "-5 -5 " ++ String.fromInt (totalSize + 10) ++ " " ++ String.fromInt (totalSize + 10)
    in
    List.concat [ bumpers, headerDice, footerDice, leftDice, rightDice, lines ]
        |> svg
            [ Svg.Attributes.width totalSizeStr
            , Svg.Attributes.height totalSizeStr
            , viewBox viewBoxStr
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
