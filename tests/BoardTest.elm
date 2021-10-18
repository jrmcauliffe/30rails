module BoardTest exposing (..)

import Board exposing (validMove)
import Expect exposing (Expectation)
import Test exposing (..)
import Types exposing (..)


marks =
    [ Mountain, Mine ]


suite =
    describe "Valid Moves"
        [ test "Cannot place any mark in status New " <|
            \_ ->
                List.map (\m -> validMove ( 1, 1 ) m New Board.init 1) marks
                    |> List.filter (\x -> x)
                    |> List.length
                    |> Expect.equal 0
        ]
