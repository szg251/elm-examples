module Style exposing (..)

import Time exposing (second)
import Css exposing (..)
import Css.Transitions as T exposing (transition)
import Html.Styled.Attributes exposing (css)


toRgba color alpha =
    rgba color.red color.green color.blue alpha



{- Theme -}


theme =
    { mainBg = hex "3498db"
    , mainColor = hex "fff"
    , formBg = hex "eee"
    , formColor = hex "808080"
    , formHoverBg = hex "f00"
    , formHoverColor = hex "fff"
    , formDisabledBg = hex "eee"
    , formDisabledColor = hex "d3d3d3"
    , borderColor = hex "eee"
    }



{- Base -}


footer =
    css
        [ position fixed
        , bottom (px 0)
        , fontSize (Css.rem 0.5)
        , textAlign center
        , width (pct 100)
        , margin2 (px 0) auto
        ]


container =
    css
        [ maxWidth (px 800)
        , margin2 (px 0) auto
        ]


elementPop =
    batch
        [ transform (scale 1.3)
        , boxShadow4 (px 0) (px 0) (px 8) (rgba 0 0 0 0.5)
        , zIndex (int 500)
        , transition
            [ T.boxShadow (0.5 * second)
            , T.transform (0.5 * second)
            ]
        ]


elementUnpop =
    batch
        [ transform (scale 1)
        , boxShadow none
        , transition
            [ T.boxShadow (0.3 * second)
            , T.transform (0.3 * second)
            ]
        ]



{- Navbar -}


nav =
    css
        [ width (pct 100)
        , fontWeight bold
        , backgroundColor theme.mainBg
        , padding (Css.rem 0.5)
        ]


nav__container =
    css
        [ maxWidth (px 800)
        , margin2 (px 0) auto
        , displayFlex
        , alignItems center
        ]


nav__title =
    css
        [ color theme.mainColor
        , fontFamilies [ "Arial", "sans-serif" ]
        , margin (px 0)
        , fontSize (Css.rem 2)
        ]



{- Form -}


form =
    css [ margin auto ]


formElement =
    batch
        [ height (Css.rem 2)
        , backgroundColor theme.formBg
        , borderStyle none
        ]


form__txt =
    css
        [ formElement
        , padding2 (px 0) (Css.rem 0.5)
        , color theme.formColor
        ]


form__btn =
    css
        [ formElement
        , color theme.formColor
        , padding (px 5)
        , elementUnpop
        , hover
            [ backgroundColor theme.formHoverBg
            , color theme.formHoverColor
            , elementPop
            ]
        , disabled
            [ backgroundColor theme.formDisabledBg
            , color theme.formDisabledColor
            , elementUnpop
            ]
        ]


menu =
    css
        [ display inlineBlock
        , listStyle none
        ]


title =
    css
        [ fontSize (em 2) ]



{- Todo style -}


todo__li =
    css
        [ listStyle none
        , borderBottom3 (px 1) solid theme.borderColor
        , displayFlex
        , alignItems left
        ]


todo__chkbox =
    css [ margin (Css.rem 0.5) ]


flex__right =
    css [ marginLeft auto ]
