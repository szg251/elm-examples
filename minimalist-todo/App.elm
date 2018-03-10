module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)


-- MODEL


type alias Todo =
    { key : Int
    , word : String
    , done : Bool
    }


type alias Model =
    { todoInput : String
    , todos : List Todo
    }


model : Model
model =
    { todoInput = ""
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
            { model | todoInput = newtodo }

        SubmitTodo ->
            let
                newtodo =
                    { key = uniqueId (List.map .key model.todos)
                    , word = model.todoInput
                    , done = False
                    }
            in
                { model
                    | todoInput = ""
                    , todos = newtodo :: model.todos
                }

        DelTodo key ->
            { model | todos = List.filter (\todo -> todo.key /= key) model.todos }

        ToggleTodo key ->
            let
                toggleTodo todo =
                    if todo.key == key then
                        { todo | done = not todo.done }
                    else
                        todo
            in
                { model | todos = List.map toggleTodo model.todos }


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
        [ Html.form [ onSubmit SubmitTodo ]
            [ input
                [ type_ "text"
                , value model.todoInput
                , onInput InputTodo
                ]
                []
            , button
                [ disabled (model.todoInput == "") ]
                [ text "OK" ]
            ]
        , div []
            [ ul [] (List.map viewListElem model.todos) ]
        ]


viewListElem : Todo -> Html Msg
viewListElem { key, word, done } =
    li []
        [ input
            [ type_ "checkbox"
            , checked done
            , onClick (ToggleTodo key)
            ]
            []
        , div [] [ text word ]
        , button
            [ onClick (DelTodo key)
            , disabled (not done)
            ]
            [ text "Delete" ]
        ]



-- MAIN


main =
    beginnerProgram { model = model, view = view, update = update }
