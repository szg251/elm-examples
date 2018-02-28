module App exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onClick, onSubmit)
import List.Extra exposing (updateIf)
import Style


main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Todo =
    { index : Int
    , word : String
    , done : Bool
    }


type alias Model =
    { newtodo : String
    , todos : List Todo
    }


model : Model
model =
    { newtodo = ""
    , todos = []
    }



-- UPDATE


type Msg
    = InputTodo String
    | SubmitTodo
    | DelTodo Int
    | ToggleTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputTodo newtodo ->
            { model | newtodo = newtodo }

        SubmitTodo ->
            let
                newtodo : Todo
                newtodo =
                    { index = uniqueId (List.map .index model.todos)
                    , word = model.newtodo
                    , done = False
                    }
            in
                { model | newtodo = "", todos = newtodo :: model.todos }

        DelTodo index ->
            { model | todos = List.filter (\todo -> todo.index /= index) model.todos }

        ToggleTodo index ->
            let
                toggleTodo : Todo -> Todo
                toggleTodo todo =
                    { todo | done = not todo.done }
            in
                { model | todos = List.Extra.updateIf (\n -> n.index == index) toggleTodo model.todos }


uniqueId : List Int -> Int
uniqueId list =
    let
        uniqueId_ : Int -> Int
        uniqueId_ newId =
            if List.all (\id -> id /= newId) list then
                newId
            else
                uniqueId_ (newId + 1)
    in
        uniqueId_ 0



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header model
        , div [ Style.container ]
            [ todoList model.todos ]
        , footer
        ]


header : Model -> Html Msg
header model =
    div [ Style.nav ]
        [ div [ Style.nav__container ]
            [ div [ Style.nav__title ] [ text "Todo App" ]
            , Html.Styled.form [ Style.form, onSubmit SubmitTodo ]
                [ input
                    [ Style.form__txt
                    , type_ "text"
                    , placeholder "todo"
                    , value model.newtodo
                    , onInput InputTodo
                    ]
                    []
                , button
                    [ Style.form__btn, disabled (model.newtodo == "") ]
                    [ text "OK" ]
                ]
            ]
        ]


todoList : List Todo -> Html Msg
todoList list =
    div []
        [ h1 [] [ text "Todo list" ]
        , ul [] (List.map listElem list)
        ]


listElem : Todo -> Html Msg
listElem { index, word, done } =
    li [ Style.todo__li ]
        [ input
            [ Style.todo__chkbox
            , type_ "checkbox"
            , checked done
            , onClick (ToggleTodo index)
            ]
            []
        , div [] [ text word ]
        , button
            [ Style.form__btn
            , Style.flex__right
            , onClick (DelTodo index)
            , disabled (not done)
            ]
            [ text "Delete" ]
        ]


footer : Html Msg
footer =
    div [ Style.footer ]
        [ text "Copyright (C) 2018 Yahoo Japan Corporation. All Rights Reserved." ]
