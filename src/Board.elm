module Board exposing (Board, clearPos, getPos, init, newState, setPos, validMove, viewBoard)

import Array exposing (Array)
import Element exposing (Element, column, el, height, padding, px, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Msg exposing (..)
import Tuple exposing (first, second)


boardSize =
    6


init : Board
init =
    Board Nothing Nothing Nothing Nothing False <| Array.repeat (boardSize * boardSize) Empty


getPos : Board -> Position -> Maybe Mark
getPos board position =
    Array.get ((boardSize * (first position - 1)) + (second position - 1)) board.playArea


setPos : Board -> GameState -> Position -> Mark -> Board
setPos board state position mark =
    case state of
        ( phase, Place roll ) ->
            { board | playArea = Array.set ((boardSize * (first position - 1)) + (second position - 1)) mark board.playArea }

        ( _, Roll ) ->
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
        |> row []


viewSpace : Board -> Position -> Element Msg
viewSpace board position =
    let
        r =
            first position

        c =
            second position

        v =
            getPos board position
    in
    button [ width <| px 60, height <| px 60, Border.color <| rgb255 0 0 0, Border.width 2, padding 5 ] <|
        case v of
            --Just (Track n) ->
            --    { onPress = Just (GotBoardClick position), label = text (String.fromInt n) }
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
    , skippedMine : Bool
    , playArea : PlayArea
    }


type alias PlayArea =
    Array Mark


newState : GameState -> GameState
newState state =
    case state of
        ( _, Roll ) ->
            state

        ( PlaceMountains n, _ ) ->
            if n < 6 then
                ( PlaceMountains (n + 1), Roll )

            else
                ( PlaceMine, Place 1 )

        ( PlaceMine, _ ) ->
            ( PlaceMine, Roll )

        _ ->
            ( PlaceMine, Roll )


validMove : Position -> Mark -> GameState -> Int -> Board -> Bool
validMove position mark phase roll board =
    case ( phase, mark, position ) of
        ( ( New, _ ), _, _ ) ->
            False

        ( ( PlaceMountains row, _ ), Mountain, ( r, _ ) ) ->
            (r == row)
                && (List.range 1 boardSize
                        |> List.filterMap (\col -> getPos board ( row, col ))
                        |> List.filter (\x -> x /= Empty)
                        |> List.length
                        |> (==) 0
                   )

        ( ( PlaceMine, _ ), Mine, _ ) ->
            True

        --( ( Main, _ ), Track i, _ ) ->
        --    True
        ( ( _, _ ), _, _ ) ->
            False
