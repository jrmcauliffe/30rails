module Board exposing (Board, clearPos, getPos, init, nextGamePhase, nextTurnPhase, renderBoard, setPos, validMove)

import Array exposing (Array)
import Dice exposing (Color(..), DiceConfig)
import Element exposing (Element)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tiles exposing (drawTile)
import Tuple exposing (first, second)
import Types exposing (..)


boardSize =
    6


{-| Every dimension of the drawing, in SVG user units. Nothing here is in
pixels: the whole board is laid out in this space and the viewBox scales it to
whatever size the caller asks for, so `cell` is an arbitrary reference unit.

The drawing is a square of `cells` x `cells` cells. The outermost ring holds the
row/column dice, the next ring in holds the grey station bumpers, and the
remaining 6x6 in the middle (starting at `playOrigin`) is the play area.

-}
type alias Geometry =
    { cell : Float
    , cells : Int
    , playOrigin : Int
    , diceInset : Float
    , pad : Float
    , stroke : Float
    }


geometry : Geometry
geometry =
    let
        cell =
            100
    in
    { cell = cell
    , cells = boardSize + 4
    , playOrigin = 2
    , diceInset = cell / 20
    , pad = cell / 12
    , stroke = cell / 30
    }


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


{-| Render the board at `pixelSize` x `pixelSize` CSS pixels. The drawing itself
is resolution-independent; the caller decides how big it should be.
-}
renderBoard : Int -> Board -> Element Msg
renderBoard pixelSize board =
    let
        g =
            geometry

        {- Cell index -> user units. -}
        at : Int -> Float
        at n =
            toFloat n * g.cell

        f : Float -> String
        f =
            String.fromFloat

        lastCell =
            g.cells - 1

        diceConfig =
            { size = g.cell - (g.diceInset * 2)
            , colors = { background = Dice.white, pips = Dice.black, border = Just Dice.black }
            }

        {- A die filling the cell at (col, row), inset a little. -}
        dieAt : Int -> Int -> Int -> Svg Msg
        dieAt col rowIndex n =
            Dice.glyph diceConfig
                { x = at col + g.diceInset, y = at rowIndex + g.diceInset }
                n

        -- Not yet rendered: tiles still need their play-area offset sorting out.
        positions =
            List.range 1 boardSize
                |> List.concatMap (\r -> List.range 1 boardSize |> List.map (\c -> ( r, c )))
                |> List.filterMap
                    (\pos -> getPos board pos |> Maybe.map (drawTile pos g.cell))

        bumpers =
            let
                drawbox : Position -> Position -> Svg Msg
                drawbox from to =
                    Svg.rect
                        [ x (f (at (first from)))
                        , y (f (at (second from)))
                        , Svg.Attributes.width (f (at (first to - first from + 1)))
                        , Svg.Attributes.height (f (at (second to - second from + 1)))
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
            List.range 1 boardSize |> List.map (\n -> dieAt (n + 1) 0 n)

        footerDice =
            List.range 1 boardSize |> List.map (\n -> dieAt (n + 1) lastCell n)

        leftDice =
            List.range 1 boardSize |> List.map (\n -> dieAt 0 (n + 1) n)

        rightDice =
            List.range 1 boardSize |> List.map (\n -> dieAt lastCell (n + 1) n)

        lines =
            List.range 0 boardSize
                |> List.concatMap
                    (\n ->
                        let
                            offset =
                                f (at (n + g.playOrigin))

                            far =
                                f (at g.cells)
                        in
                        [ Svg.line [ x1 "0", y1 offset, x2 far, y2 offset, stroke "black" ] []
                        , Svg.line [ y1 "0", x1 offset, y2 far, x2 offset, stroke "black" ] []
                        ]
                    )

        viewBoxStr =
            String.join " "
                [ f -g.pad
                , f -g.pad
                , f (at g.cells + (g.pad * 2))
                , f (at g.cells + (g.pad * 2))
                ]
    in
    List.concat [ bumpers, headerDice, footerDice, leftDice, rightDice, lines ]
        |> svg
            [ viewBox viewBoxStr
            , Svg.Attributes.class "board"
            , Svg.Attributes.width (String.fromInt pixelSize)
            , Svg.Attributes.height (String.fromInt pixelSize)
            , preserveAspectRatio "xMidYMid meet"
            , strokeWidth (f g.stroke)
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
