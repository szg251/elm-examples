module Tea1 exposing (..)

import Html exposing (beginnerProgram, text)


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model =
    "World"


view model =
    text ("Hello " ++ model)


update msg model =
    model
