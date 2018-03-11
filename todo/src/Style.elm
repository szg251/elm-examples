module Style exposing (..)

import Css exposing (..)
import Html.Styled.Attributes exposing (css)


{- Colors -}


colors =
    { mainBg = hex "3498db"
    , mainFg = hex "fff"
    , formBg = hex "eee"
    , formFg = hex "808080"
    , formHoverBg = hex "f00"
    , formHoverFg = hex "fff"
    , formDisabledBg = hex "eee"
    , formDisabledFg = hex "d3d3d3"
    , borders = hex "eee"
    }



{- Base -}


footer =
    css
        [ position fixed
        , bottom (px 0)
        , fontSize (Css.rem 0.5)
        , textAlign center
        , width (vw 100)
        , margin2 (px 0) auto
        ]


container =
    css
        [ maxWidth (px 800)
        , margin2 (px 0) auto
        ]



{- Navbar -}


nav =
    css
        [ width (vw 100)
        , fontWeight bold
        , backgroundColor colors.mainBg
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
        [ color colors.mainFg
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
        , backgroundColor colors.formBg
        , borderStyle none
        ]


form__txt =
    css
        [ formElement
        , padding2 (px 0) (Css.rem 0.5)
        , color colors.formFg
        ]


form__btn =
    css
        [ formElement
        , color colors.formFg
        , padding (px 5)
        , hover
            [ backgroundColor colors.formHoverBg
            , color colors.formHoverFg
            ]
        , disabled
            [ backgroundColor colors.formDisabledBg
            , color colors.formDisabledFg
            ]
        ]


menu =
    css
        [ display inlineBlock
        , listStyle none
        ]


title =
    css
        [ fontSize (em 2)
        ]



{- Todo style -}


todo__li =
    css
        [ listStyle none
        , borderBottom3 (px 1) solid colors.borders
        , displayFlex
        , alignItems left
        ]


todo__chkbox =
    css [ margin (Css.rem 0.5) ]


flex__right =
    css [ marginLeft auto ]
