module Utils exposing (..)


uniqueId : List Int -> Int
uniqueId list =
    let
        uniqueId_ : Int -> Int
        uniqueId_ newId =
            if List.all (\id -> id /= (newId)) list then
                newId
            else
                uniqueId_ (newId + 1)
    in
        uniqueId_ 0
