module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, el, height, px, rgb255, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Random


type Msg
    = ClickedRoll
    | GotDiceIndex Int


view : Model -> Html Msg
view model =
    Element.layout [ Font.size 50 ] <|
        column [ centerX ]
            [ text "30 Rails"
            , row []
                [ viewBoard model, viewPanel model ]
            ]


viewBoard : Model -> Element Msg
viewBoard model =
    List.range 1 6
        |> List.map (\r -> el [] <| viewRow r model)
        |> column []


viewRow : Int -> Model -> Element Msg
viewRow r model =
    List.range 1 6
        |> List.map (\c -> viewSpace (Dict.get ( r, c ) model.board.playArea))
        |> row []


viewSpace : Maybe Int -> Element Msg
viewSpace v =
    el [ width <| px 60, height <| px 60, Border.color <| rgb255 0 0 0, Border.width 5 ] <|
        case v of
            Just a ->
                text (String.fromInt a)

            Nothing ->
                text ""


viewPanel model =
    column []
        [ button []
            { onPress = Just ClickedRoll
            , label = text "Roll"
            }
        , viewFace model
        ]


viewFace model =
    text <| String.fromInt model.face


type alias Board =
    { n : Maybe Int
    , s : Maybe Int
    , e : Maybe Int
    , w : Maybe Int
    , playArea : Dict ( Int, Int ) Int
    }


type alias Model =
    { face : Int
    , board : Board
    }


initialModel : Model
initialModel =
    { face = 1
    , board =
        Dict.singleton ( 1, 1 ) 1
            |> Dict.insert ( 1, 2 ) 3
            |> Board Nothing Nothing Nothing Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedRoll ->
            Random.int 1 6
                |> Random.generate GotDiceIndex
                |> Tuple.pair model

        GotDiceIndex face ->
            ( { model | face = face }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
