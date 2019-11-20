module ThirtyRails exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type Msg
    = ClickedRoll


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "30Rails" ]
        , button [ onClick ClickedRoll ] [ text "Roll!" ]
        , viewDice model.face
        ]


viewDice : Int -> Html Msg
viewDice face =
    div [] [ text (String.fromInt face) ]


type alias Model =
    { face : Int }


initialModel : Model
initialModel =
    { face = 1 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedRoll ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
