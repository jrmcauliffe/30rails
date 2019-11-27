module ThirtyRails exposing (main)

import Browser
import Element exposing (Element, centerX, el, row, text)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type Msg
    = ClickedRoll
    | GotDiceIndex Int


view : Model -> Html Msg
view model =
    Element.layout [ Font.size 50 ] <|
        row [ centerX ]
            [ text "30 Rails"
            , button []
                { onPress = Just ClickedRoll
                , label = text "Roll"
                }
            , viewFace model
            ]


viewFace model =
    text <| String.fromInt model.face


type alias Model =
    { face : Int }


initialModel : Model
initialModel =
    { face = 1 }


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
