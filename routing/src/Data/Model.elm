module Data.Model exposing (..)

import Dict exposing (Dict)
import Data.Routes as Routes exposing (Page, UserRole(..))


type alias Model =
    { currentUrl : Page
    , loginName : String
    , loginPass : String
    , currentUser : Maybe ( String, List UserRole )
    , users : Dict String { pass : String, roles : List UserRole }
    , profiles : Dict String UserProfile
    }


type alias UserProfile =
    { fullname : String
    , age : Int
    }


initModel : Model
initModel =
    { currentUrl = Routes.HomePage
    , loginName = ""
    , loginPass = ""
    , currentUser = Nothing
    , users =
        Dict.fromList
            [ ( "gege251", { pass = "1234", roles = [ Normal, Admin ] } )
            , ( "takeda", { pass = "2345", roles = [ Normal ] } )
            , ( "kovacs", { pass = "3456", roles = [ Normal ] } )
            ]
    , profiles =
        Dict.fromList
            [ ( "gege251", { fullname = "Szabo Gergely", age = 20 } )
            , ( "takata", { fullname = "Takada Tarou", age = 25 } )
            , ( "kovacs", { fullname = "Kovacs Jeno", age = 50 } )
            ]
    }
