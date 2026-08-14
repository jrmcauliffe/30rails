module Types exposing (GamePhase(..), Mark(..), Msg(..), Position, TurnPhase(..), Viewport, getMark)


type alias Position =
    ( Int, Int )


{-| Size of the browser window in CSS pixels. Seeded from flags so the first
render is already correctly sized, then kept current by Browser.Events.onResize.
-}
type alias Viewport =
    { width : Int
    , height : Int
    }


type Mark
    = Mountain
    | Mine
    | Empty


getMark : GamePhase -> Mark
getMark phase =
    case phase of
        PlaceMountains n ->
            Mountain

        PlaceMine ->
            Mine

        _ ->
            Empty


type GamePhase
    = New
    | PlaceMountains Int
    | PlaceMine
    | PlaceStations Int
    | PlaceBonus
    | Main
    | Gameover
    | Error String


type TurnPhase
    = Roll
    | Place Int


type Msg
    = ClickedRoll
    | ClickedStart
    | GotDiceIndex Int
    | GotBoardClick Position
    | WindowResized Int Int
