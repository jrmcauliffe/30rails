module Main exposing (main)

import Board exposing (..)
import Browser
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, padding, row, text, el)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Msg exposing (..)
import Random
import Tuple exposing (first, second)

hints : List (Phase, String)
hints = [(New, "Hit start to begin a game"),(PlaceMountains, "Roll the white die 6 times, once for each row of the \"map\". Draw a mountain symbol in the column corresponding to the number on the die. One die roll may be ignored, and a mountain is not drawn in this row."),(PlaceMine, "Select one space that is orthogonally adjacent to a mountain and write a letter \"M\" in that space. This represents a mine."),(PlaceStations, "Write each of the numbers from 1 to 4 in one of the grey squares around the edge of the map. One number must be written on each of the four sides of the map. Each number represents a station. You will score more for connecting the higher numbered stations."),(PlaceBonus, "Highlight one \"bonus\" square on the map, within the 6x6 white area in the centre. This may be by lightly shading; by marking a corner; or by drawing around the outline."),(Main, ""), (Gameover, ""),(Error, "An error has occurred :-(")]

view : Model -> Html Msg
view model =
    Element.layout [ ] <|
        row [ padding 30 ]
            [ column
                    [ padding 30, centerX ]
                    [ el [ Font.size 50 ] (text "30 Rails"), viewBoard model.board, viewHint model.phase ]
            , viewPanel model
            ]


viewPanel model =
    column [ padding 60 ]
        [ button [ Font.size 30 ]
            { onPress = Just ClickedRoll
            , label = text "Roll"
            }
        , viewFace model.face
        ]


viewFace : Int -> Element Msg
viewFace face =
    el [ Font.size 15 ] (text <| String.fromInt face)

viewHint : Phase -> Element Msg
viewHint phase = 
  List.filter (\p -> first p == phase) hints    
  |> List.head
  |> Maybe.map (\x -> second x) 
  |> Maybe.withDefault("Error")
  |> text

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
