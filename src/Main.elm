module Main exposing (main)

import Board exposing (..)
import Browser
import Browser.Events
import Dice exposing (Color(..), DiceConfig)
import Element exposing (Element, centerX, column, el, row, text)
import Element.Font as Font
import Element.Input exposing (button)
import Hints exposing (getHint)
import Html exposing (Html)
import Layout exposing (Metrics)
import Random
import Types exposing (..)


view : Model -> Html Msg
view model =
    let
        m =
            Layout.fromViewport model.viewport
    in
    Layout.page m
        [ viewBoard m model
        , viewPanel m model
        ]


viewBoard : Metrics -> Model -> Element Msg
viewBoard m model =
    Layout.stack m
        [ el
            [ Font.size m.titleFont, Font.family [ Font.typeface "Alfa Slab One" ], centerX ]
            (text "30 Rails")
        , renderBoard m.board model.board
        , viewHint m model.gamePhase
        , viewDebug m model
        ]


viewPanel : Metrics -> Model -> Element Msg
viewPanel m model =
    let
        v =
            case ( model.gamePhase, model.turnPhase ) of
                ( New, _ ) ->
                    button [ Font.size m.buttonFont, centerX ]
                        { onPress = Just ClickedStart
                        , label = text "Start"
                        }

                ( _, Roll ) ->
                    button [ Font.size m.buttonFont, centerX ]
                        { onPress = Just ClickedRoll
                        , label = text "Roll"
                        }

                _ ->
                    Element.none
    in
    Layout.panel m
        [ v
        , viewFace m model.face
        ]


diceConfig : Metrics -> DiceConfig
diceConfig m =
    { size = m.panelDice
    , colors = { background = Named "navy", pips = Named "red", border = Nothing }
    }


viewFace : Metrics -> Int -> Element Msg
viewFace m face =
    Dice.view (diceConfig m) face |> Element.html


{-| A paragraph rather than an `el`: hints run to a couple of hundred
characters, and an `el` sizes to its content, so the text would run off the side
of the window instead of wrapping.
-}
viewHint : Metrics -> GamePhase -> Element Msg
viewHint m phase =
    Element.paragraph
        [ Font.size m.bodyFont, Layout.boardWidth m, Layout.lineSpacing m ]
        [ text (getHint phase) ]


viewDebug : Metrics -> Model -> Element Msg
viewDebug m model =
    column [ Font.size m.bodyFont, Layout.boardWidth m ]
        [ row [] [ text "Debug" ]
        , row [] [ text (gamePhaseString model.gamePhase) ]
        , row [] [ text (turnPhaseString model.turnPhase) ]
        ]


type alias Model =
    { face : Int
    , gamePhase : GamePhase
    , turnPhase : TurnPhase
    , board : Board
    , viewport : Viewport
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


{-| Window size comes in as flags rather than via Browser.Dom.getViewport, so
the very first render is already the right size instead of flashing at a
default and then resizing.
-}
type alias Flags =
    { width : Int
    , height : Int
    }


initialModel : Flags -> Model
initialModel flags =
    { face = 1
    , gamePhase = New
    , turnPhase = Roll
    , board = Board.init
    , viewport = { width = flags.width, height = flags.height }
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

        WindowResized w h ->
            ( { model | viewport = { width = w, height = h } }, Cmd.none )



-- model


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel flags, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Browser.Events.onResize WindowResized
        }
