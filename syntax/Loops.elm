module Loops exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main =
    div []
        [ text ("Sum: " ++ toString (sum [ 1, 2, 3 ]))
        , br [] []
        , text ("Sum_: " ++ toString (sum_ [ 1, 2, 3 ]))
        , br [] []
        , text ("Double: " ++ toString (double [ 1, 2, 3 ]))
        , br [] []
        , text ("Double_: " ++ toString (double_ [ 1, 2, 3 ]))
        , br [] []
        , text ("Prefix: " ++ toString (prefix [ 1, 2, 3 ]))
        , br [] []
        , select [] (options [ 1, 2, 3 ])
        , br [] []
        , text ("Recursion: " ++ toString (recursion 3))
        ]


sum : List Int -> Int
sum list =
    List.foldl (\x y -> x + y) 0 list


sum_ : List Int -> Int
sum_ list =
    List.foldl (+) 0 list


double : List Int -> List Int
double list =
    List.map (\x -> x * 2) list


double_ : List Int -> List Int
double_ list =
    List.map ((*) 2) list


prefix : List Int -> List String
prefix list =
    List.map (\num -> "num" ++ toString num) list


options : List Int -> List (Html msg)
options list =
    List.map (\num -> option [ value (toString num) ] [ text ("option: " ++ toString num) ]) list


recursion : Int -> Int
recursion i =
    if i <= 0 then
        0
    else
        2 + (recursion (i - 1))
