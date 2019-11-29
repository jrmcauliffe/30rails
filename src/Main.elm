module Main exposing (main)

import Board exposing (..)
import Browser
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, padding, row, text)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Msg exposing (..)
import Random


view : Model -> Html Msg
view model =
    Element.layout [ Font.size 50 ] <|
        row [ padding 30 ]
            [ column
                [ padding 40 ]
                [ column
                    [ padding 30, centerX ]
                    [ text "30 Rails", viewBoard model.board ]
                ]
            , viewPanel model
            ]


viewPanel model =
    column [ padding 60 ]
        [ button []
            { onPress = Just ClickedRoll
            , label = text "Roll"
            }
        , viewFace model.face
        ]


viewFace : Int -> Element Msg
viewFace face =
    text <| String.fromInt face


type alias Model =
    { face : Int
    , board : Board
    }


initialModel : Model
initialModel =
    { face = 1
    , board = Board.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedRoll ->
            Random.int 1 6
                |> Random.generate GotDiceIndex
                |> Tuple.pair model

        GotDiceIndex face ->
            let
                oldboard =
                    model.board

                newboard =
                    { oldboard | sr = Just face }
            in
            ( { model | face = face, board = newboard }, Cmd.none )

        GotBoardClick position ->
            ( { model | board = Board.setPos model.board position Mine }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
