module BoardTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite = describe "Board State" [
    test "First Test" <| \_ -> (1 + 1) |> Expect.equal 2
    ,test "Second Test" <| \_ -> (1 + 1) |> Expect.equal 2
    ]