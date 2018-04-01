module UtilsTest exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Utils exposing (..)


-- STUBS
-- TESTS


myfirsttest : Test
myfirsttest =
    describe "Utils.uniqueId"
        [ test "Next one in a sequence" <|
            \_ ->
                uniqueId [ "0", "1", "2" ]
                    |> Expect.equal "3"
        , test "Filling a hole" <|
            \_ ->
                uniqueId [ "0", "1", "3" ]
                    |> Expect.equal "2"
        ]
