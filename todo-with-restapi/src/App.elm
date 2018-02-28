module App exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onClick, onSubmit)
import RemoteData exposing (WebData, toMaybe)
import List.Extra exposing (updateIf)
import Dict exposing (Dict)
import Dict.Extra
import Style
import Utils exposing (..)
import Model exposing (..)
import Msgs exposing (..)
import Requests exposing (..)


-- INIT


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchTodos )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputTodo newtodo ->
            ( { model | newtodo = newtodo }, Cmd.none )

        SubmitTodo ->
            case RemoteData.toMaybe model.todos of
                Nothing ->
                    ( model, Cmd.none )

                Just todos ->
                    let
                        newtodo =
                            { id = uniqueId (Dict.keys todos)
                            , value = model.newtodo
                            , done = False
                            , order = Dict.size todos
                            }

                        newtodos =
                            Dict.insert newtodo.id newtodo todos
                    in
                        ( { model | newtodo = "", todos = RemoteData.Success newtodos }
                        , putNewTodo newtodo
                        )

        DelTodo id ->
            case RemoteData.toMaybe model.todos of
                Nothing ->
                    ( model, Cmd.none )

                Just todos ->
                    let
                        newtodos =
                            Dict.remove id todos
                    in
                        ( { model | todos = RemoteData.Success newtodos }, deleteTodo id )

        ToggleTodo id ->
            case RemoteData.toMaybe model.todos of
                Nothing ->
                    ( model, Cmd.none )

                Just todos ->
                    let
                        newtodo =
                            Dict.get id todos
                                |> Maybe.map toggleTodo

                        toggleTodo todo =
                            { todo | done = not todo.done }
                    in
                        case newtodo of
                            Nothing ->
                                ( model, Cmd.none )

                            Just todo ->
                                ( { model
                                    | todos =
                                        todos
                                            |> Dict.insert id todo
                                            |> RemoteData.Success
                                  }
                                , patchTodo todo
                                )

        AfterFetchTodos response ->
            ( { model | todos = RemoteData.map (Dict.Extra.fromListBy .id) response }, Cmd.none )

        AfterPutNewTodo oldId response ->
            case RemoteData.toMaybe (RemoteData.map2 (,) model.todos response) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( todos, newtodo ) ->
                    let
                        setId todo =
                            { todo | id = newtodo.id }

                        newtodos =
                            todos
                                |> Dict.remove oldId
                                |> Dict.insert newtodo.id newtodo
                    in
                        ( { model | todos = RemoteData.Success newtodos }, Cmd.none )

        AfterDeleteTodo todo ->
            ( model, Cmd.none )

        AfterPatchTodo todo ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header model
        , div [ Style.container ]
            [ h1 [] [ text "Todo list" ]
            , todoList model.todos
            ]
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


todoList : WebData (Dict String Todo) -> Html Msg
todoList todos =
    case todos of
        RemoteData.NotAsked ->
            text "Initializing..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success list ->
            case Dict.size list of
                0 ->
                    text "No items."

                otherwise ->
                    ul []
                        (list
                            |> Dict.values
                            |> List.sortBy .order
                            |> List.map listElem
                        )

        RemoteData.Failure error ->
            text "Could not fetch todos."


listElem : Todo -> Html Msg
listElem { id, value, done } =
    li [ Style.todo__li ]
        [ input
            [ Style.todo__chkbox
            , type_ "checkbox"
            , checked done
            , onClick (ToggleTodo id)
            ]
            []
        , div [] [ text value ]
        , button
            [ Style.form__btn
            , Style.flex__right
            , onClick (DelTodo id)
            , disabled (not done)
            ]
            [ text "Delete" ]
        ]


footer : Html Msg
footer =
    div [ Style.footer ]
        [ text "Copyright (C) 2018 Yahoo Japan Corporation. All Rights Reserved." ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
