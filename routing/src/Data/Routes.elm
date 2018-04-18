module Data.Routes exposing (..)

import UrlParser as Url exposing ((</>), Parser, oneOf, s, string)


type Page
    = HomePage
    | LoginPage (Maybe String)
    | RestrictedPage UserRole RestrictedPage


type RestrictedPage
    = ProfilePage String
    | AdminPage


type UserRole
    = Normal
    | Admin


router : Parser (Page -> a) a
router =
    oneOf
        [ Url.map HomePage (s "main")
        , Url.map (LoginPage Nothing) (s "login")
        , Url.map (LoginPage << Just) (s "login" </> string)
        , Url.map (RestrictedPage Normal << ProfilePage) (s "profile" </> string)
        , Url.map (RestrictedPage Admin AdminPage) (s "admin")
        ]
