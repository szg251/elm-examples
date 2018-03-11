module DecoderTest exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Model exposing (..)
import Json.Decode as Decode
import Result


decoderTests : Test
decoderTests =
    describe "Decoders"
        [ test "todosDecoder" <|
            \_ ->
                let
                    testJson =
                        """
                    [
                        {
                            "id": "123",
                            "value": "val",
                            "done": false
                        },
                        {
                            "id": "234",
                            "value": "valb",
                            "done": true
                        }
                    ]
                    """

                    expectedData =
                        Result.Ok
                            [ { id = "123"
                              , value = "val"
                              , done = False
                              }
                            , { id = "234"
                              , value = "valb"
                              , done = True
                              }
                            ]

                    decoded =
                        Decode.decodeString todosDecoder testJson
                in
                    Expect.equal expectedData decoded
        ]
