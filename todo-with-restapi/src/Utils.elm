module Utils exposing (..)


uniqueId : List String -> String
uniqueId list =
    let
        uniqueId_ : Int -> String
        uniqueId_ newId =
            if List.all (\id -> id /= (toString newId)) list then
                toString newId
            else
                uniqueId_ (newId + 1)
    in
        uniqueId_ 0
