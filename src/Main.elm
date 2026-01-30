module Main exposing (main)

import Board exposing (..)
import Browser
import Dice exposing (Color(..), DiceColors, DiceConfig, view)
import Element exposing (Element, centerX, column, el, padding, row, text)
import Element.Font as Font
import Element.Input exposing (button)
import Hints exposing (getHint)
import Html exposing (Html)
import Random
import Svg exposing (Svg)
import Types exposing (..)


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ padding 30 ]
            [ column
                [ padding 30, centerX ]
                [ el [ Font.size 50 ] (text "30 Rails"), renderBoard model.board, viewHint model.gamePhase, viewDebug model ]
            , viewPanel model
            ]


viewPanel : Model -> Element Msg
viewPanel model =
    let
        v =
            case ( model.gamePhase, model.turnPhase ) of
                ( New, _ ) ->
                    button [ Font.size 30 ]
                        { onPress = Just ClickedStart
                        , label = text "Start"
                        }

                ( _, Roll ) ->
                    button [ Font.size 30 ]
                        { onPress = Just ClickedRoll
                        , label = text "Roll"
                        }

                _ ->
                    Element.none
    in
    column [ padding 60 ]
        [ v
        , viewFace model.face
        ]


diceConfig : DiceConfig
diceConfig =
    { size = 56
    , colors = { background = Named "navy", pips = Named "red", border = Nothing }
    }


viewFace : Int -> Element Msg
viewFace face =
    Dice.view diceConfig face |> Element.html



--    el [ Font.size 15 ] (text <| String.fromInt face)


viewHint : GamePhase -> Element Msg
viewHint phase =
    getHint phase
        |> text
        |> el [ Font.size 15 ]


viewDebug : Model -> Element Msg
viewDebug model =
    column []
        [ row [] [ text "Debug" ]
        , row [] [ text (gamePhaseString model.gamePhase) ]
        , row [] [ text (turnPhaseString model.turnPhase) ]
        ]


type alias Model =
    { face : Int
    , gamePhase : GamePhase
    , turnPhase : TurnPhase
    , board : Board
    }


turnPhaseString : TurnPhase -> String
turnPhaseString s =
    case s of
        Roll ->
            "Roll"

        Place n ->
            "Place " ++ String.fromInt n


gamePhaseString : GamePhase -> String
gamePhaseString p =
    case p of
        New ->
            "New"

        PlaceMountains i ->
            "Place Mountains, row " ++ String.fromInt i

        PlaceMine ->
            "Place Mine"

        PlaceStations _ ->
            "Place Stations"

        PlaceBonus ->
            "Place Bonus Tile"

        Main ->
            "Main"

        Gameover ->
            "Game Over"

        Error s ->
            "Error: " ++ s


initialModel : Model
initialModel =
    { face = 1
    , gamePhase = New
    , turnPhase = Roll
    , board = Board.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedRoll ->
            Random.int 1 6
                |> Random.generate GotDiceIndex
                |> Tuple.pair model

        ClickedStart ->
            ( { model | gamePhase = PlaceMountains 1, turnPhase = Roll }, Cmd.none )

        GotDiceIndex face ->
            ( { model | face = face, turnPhase = Place face }, Cmd.none )

        GotBoardClick position ->
            if Board.validMove position (getMark model.gamePhase) model.gamePhase model.board model.face then
                ( { model
                    | gamePhase = Board.nextGamePhase model.gamePhase
                    , turnPhase = Board.nextTurnPhase model.gamePhase model.turnPhase
                    , board = Board.setPos model.board position (getMark model.gamePhase)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



-- model


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
