module Hints exposing (getHint, hints)

import Msg exposing (..)
import Tuple exposing (first, second)


hints : List ( GamePhase, String )
hints =
    [ ( New, "Press start to begin a game" )
    , ( PlaceMountains 0, "Roll the white die 6 times, once for each row of the \"map\". Draw a mountain symbol in the column corresponding to the number on the die. One die roll may be ignored, and a mountain is not drawn in this row." )
    , ( PlaceMine, "Select one space that is orthogonally adjacent to a mountain and write a letter \"M\" in that space. This represents a mine." )
    , ( PlaceStations 0, "Write each of the numbers from 1 to 4 in one of the grey squares around the edge of the map. One number must be written on each of the four sides of the map. Each number represents a station. You will score more for connecting the higher numbered stations." )
    , ( PlaceBonus, "Highlight one \"bonus\" square on the map, within the 6x6 white area in the centre. This may be by lightly shading; by marking a corner; or by drawing around the outline." )
    , ( Main, "" )
    , ( Gameover, "" )
    , ( Error "", "An error has occurred :-(" )
    ]


getHint : GamePhase -> String
getHint phase =
    List.filter (\p -> first p == phase) hints
        |> List.head
        |> Maybe.map (\x -> second x)
        |> Maybe.withDefault "Error"
