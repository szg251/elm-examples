module App exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onClick, onSubmit)
import List.Extra exposing (updateIf)
import Style


-- MODEL


type alias Todo =
    { key : Int
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



-- MESSAGES


type Msg
    = InputTodo String
    | SubmitTodo
    | DelTodo Int
    | ToggleTodo Int



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputTodo newtodo ->
            { model | newtodo = newtodo }

        SubmitTodo ->
            let
                newtodo : Todo
                newtodo =
                    { key = uniqueId (List.map .key model.todos)
                    , word = model.newtodo
                    , done = False
                    }
            in
                { model
                    | newtodo = ""
                    , todos = newtodo :: model.todos
                }

        DelTodo key ->
            { model | todos = List.filter (\todo -> todo.key /= key) model.todos }

        ToggleTodo key ->
            let
                toggleTodo : Todo -> Todo
                toggleTodo todo =
                    { todo | done = not todo.done }
            in
                { model | todos = List.Extra.updateIf (\n -> n.key == key) toggleTodo model.todos }


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
        [ viewHeader model
        , div [ Style.container ]
            [ viewTodoList model.todos ]
        , viewFooter
        ]


viewHeader : Model -> Html Msg
viewHeader model =
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


viewTodoList : List Todo -> Html Msg
viewTodoList list =
    div []
        [ h1 [] [ text "Todo list" ]
        , ul [] (List.map viewListElem list)
        ]


viewListElem : Todo -> Html Msg
viewListElem { key, word, done } =
    li [ Style.todo__li ]
        [ input
            [ Style.todo__chkbox
            , type_ "checkbox"
            , checked done
            , onClick (ToggleTodo key)
            ]
            []
        , div [] [ text word ]
        , button
            [ Style.form__btn
            , Style.flex__right
            , onClick (DelTodo key)
            , disabled (not done)
            ]
            [ text "Delete" ]
        ]


viewFooter : Html Msg
viewFooter =
    div [ Style.footer ]
        [ text "This is a nice footer" ]


main : Program Never Model Msg
main =
    beginnerProgram { model = model, view = view, update = update }
