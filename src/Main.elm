module Main exposing (main)

import Board exposing (..)
import Browser
import Element exposing (Element, centerX, column, el, padding, row, text)
import Element.Font as Font
import Element.Input exposing (button)
import Hints exposing (getHint)
import Html exposing (Html)
import Msg exposing (..)
import Random
import Tuple exposing (first, second)


view : Model -> Html Msg
view model =
    Element.layout [] <|
        row [ padding 30 ]
            [ column
                [ padding 30, centerX ]
                [ el [ Font.size 50 ] (text "30 Rails"), viewBoard model.board, viewHint model.phase, viewDebug model ]
            , viewPanel model
            ]


viewPanel : Model -> Element Msg
viewPanel model =
    let
        ps =
            ( model.phase, model.state )

        v =
            case ps of
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


viewFace : Int -> Element Msg
viewFace face =
    el [ Font.size 15 ] (text <| String.fromInt face)


viewHint : Phase -> Element Msg
viewHint phase =
    getHint phase
        |> text
        |> el [ Font.size 15 ]


viewDebug : Model -> Element Msg
viewDebug model =
    row [] [ text "Debug", text (phaseString model.phase) ]


type alias Model =
    { face : Int
    , state : State
    , phase : Phase
    , board : Board
    }


phaseString : Phase -> String
phaseString p =
    case p of
        New ->
            "New"

        PlaceMountains i ->
            "Place Mountains, row " ++ String.fromInt i

        PlaceMine ->
            "Place Mine"

        PlaceStations i ->
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
    , state = Roll
    , phase = New
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
            ( { model | phase = PlaceMountains 1, state = Roll }, Cmd.none )

        GotDiceIndex face ->
            ( { model | face = face, state = Place face }, Cmd.none )

        GotBoardClick position ->
            ( { model | board = Board.setPos model.board model.phase position model.state Mountain, state = Roll }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
