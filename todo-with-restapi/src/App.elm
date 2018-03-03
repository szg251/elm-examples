module App exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onClick, onSubmit)
import RemoteData exposing (WebData, toMaybe)
import List.Extra exposing (updateIf)
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
        InputTodoField newtodo ->
            ( { model | newtodo = newtodo }, Cmd.none )

        SubmitTodo newtodo ->
            let
                ( newtodos, cmd ) =
                    case RemoteData.toMaybe model.todos of
                        Nothing ->
                            ( model.todos, Cmd.none )

                        Just todos ->
                            updateTodos (SaveTodo newtodo) todos
                                |> Tuple.mapFirst (RemoteData.succeed)
            in
                ( { model | newtodo = "", todos = newtodos }, cmd )

        ApiMsg apiMsg ->
            ( updateApi apiMsg model, Cmd.none )

        TodoMsg todoMsg ->
            case RemoteData.toMaybe model.todos of
                Nothing ->
                    ( model, Cmd.none )

                Just todos ->
                    let
                        ( newtodos, cmd ) =
                            updateTodos todoMsg todos
                                |> Tuple.mapFirst RemoteData.Success
                    in
                        ( { model | todos = newtodos }, cmd )


updateApi : ApiMsg -> Model -> Model
updateApi msg model =
    case msg of
        AfterFetchTodos response ->
            { model | todos = response }

        AfterPutNewTodo oldId response ->
            case RemoteData.toMaybe (RemoteData.map2 (,) model.todos response) of
                Nothing ->
                    model

                Just ( todos, newtodo ) ->
                    let
                        updateId todo =
                            { todo | id = newtodo.id }

                        newtodos =
                            List.Extra.updateIf (\t -> t.id == oldId) updateId todos
                    in
                        { model | todos = RemoteData.Success newtodos }

        AfterDeleteTodo todo ->
            model

        AfterPatchTodo todo ->
            model


updateTodos : TodoMsg -> List Todo -> ( List Todo, Cmd Msg )
updateTodos msg todos =
    case msg of
        SaveTodo newvalue ->
            let
                newtodo =
                    { id = uniqueId (List.map .id todos)
                    , value = newvalue
                    , done = False
                    }
            in
                ( newtodo :: todos, putNewTodo newtodo )

        DelTodo id ->
            ( List.filter (\t -> t.id /= id) todos, deleteTodo id )

        ToggleTodo id ->
            let
                newtodo =
                    List.Extra.find (\t -> t.id == id) todos
                        |> Maybe.map toggleTodo

                toggleTodo todo =
                    { todo | done = not todo.done }
            in
                case newtodo of
                    Nothing ->
                        ( todos, Cmd.none )

                    Just todo ->
                        ( List.Extra.replaceIf (\t -> t.id == id) todo todos, patchTodo todo )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [ Style.container ]
            [ h1 [] [ text "Todo list" ]
            , viewTodoList model.todos
            ]
        , viewFooter
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ Style.nav ]
        [ div [ Style.nav__container ]
            [ div [ Style.nav__title ] [ text "Todo App" ]
            , Html.Styled.form
                [ Style.form
                , onSubmit (SubmitTodo model.newtodo)
                ]
                [ input
                    [ Style.form__txt
                    , type_ "text"
                    , placeholder "todo"
                    , value model.newtodo
                    , onInput InputTodoField
                    ]
                    []
                , button
                    [ Style.form__btn, disabled (model.newtodo == "") ]
                    [ text "OK" ]
                ]
            ]
        ]


viewTodoList : WebData (List Todo) -> Html Msg
viewTodoList todos =
    case todos of
        RemoteData.NotAsked ->
            text "Initializing..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success list ->
            case List.length list of
                0 ->
                    text "No items."

                otherwise ->
                    ul [] (List.map viewListElem list)

        RemoteData.Failure error ->
            text "Could not fetch todos."


viewListElem : Todo -> Html Msg
viewListElem todo =
    li [ Style.todo__li ]
        [ input
            [ Style.todo__chkbox
            , type_ "checkbox"
            , checked todo.done
            , onClick (TodoMsg << ToggleTodo <| todo.id)
            ]
            []
        , div [] [ text todo.value ]
        , button
            [ Style.form__btn
            , Style.flex__right
            , onClick (TodoMsg << DelTodo <| todo.id)
            , disabled (not todo.done)
            ]
            [ text "Delete" ]
        ]


viewFooter : Html Msg
viewFooter =
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
