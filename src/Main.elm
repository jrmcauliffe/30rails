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
                [ el [ Font.size 50 ] (text "30 Rails"), viewBoard model.board, viewHint model.phase ]
            , viewPanel model
            ]


viewPanel : Model -> Element Msg
viewPanel model =
    let
        p =
            model.phase

        v =
            case p of
                New ->
                    button [ Font.size 30 ]
                        { onPress = Just ClickedStart
                        , label = text "Start"
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


type alias Model =
    { face : Int
    , phase : Phase
    , board : Board
    }


initialModel : Model
initialModel =
    { face = 1
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
            ( { model | phase = PlaceMountains 1 }, Cmd.none )

        GotDiceIndex face ->
            let
                oldboard =
                    model.board

                newboard =
                    { oldboard | sr = Just face }
            in
            ( { model | face = face, board = newboard }, Cmd.none )

        GotBoardClick position ->
            ( { model | board = Board.setPos model.board model.phase position model.face Mountain }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
