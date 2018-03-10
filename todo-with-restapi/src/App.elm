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
        NoOp ->
            model ! [ Cmd.none ]

        InputTodoField newtodo ->
            { model | newtodo = newtodo } ! [ Cmd.none ]

        SubmitTodo newvalue ->
            let
                ( newtodos, cmd ) =
                    RemoteData.update addTodo model.todos

                addTodo : List Todo -> ( List Todo, Cmd Msg )
                addTodo todos =
                    let
                        newtodo =
                            { id = uniqueId (List.map .id todos)
                            , value = newvalue
                            , done = False
                            }
                    in
                        (newtodo :: todos)
                            ! [ putNewTodo newtodo ]
            in
                { model | todos = newtodos, newtodo = "" } ! [ cmd ]

        DelTodo id ->
            let
                ( newtodos, cmd ) =
                    RemoteData.update delTodo model.todos

                delTodo : List Todo -> ( List Todo, Cmd Msg )
                delTodo todos =
                    List.filter (\t -> t.id /= id) todos
                        ! [ deleteTodo id ]
            in
                { model | todos = newtodos } ! [ cmd ]

        ToggleTodo id ->
            let
                ( newtodos, cmd ) =
                    RemoteData.update toggleTodo model.todos

                toggleTodo : List Todo -> ( List Todo, Cmd Msg )
                toggleTodo todos =
                    let
                        newtodo =
                            List.Extra.find (\t -> t.id == id) todos
                                |> Maybe.map (\t -> { t | done = not t.done })
                    in
                        case newtodo of
                            Nothing ->
                                todos ! [ Cmd.none ]

                            Just todo ->
                                List.Extra.replaceIf (\t -> t.id == id) todo todos
                                    ! [ patchTodo todo ]
            in
                { model | todos = newtodos } ! [ cmd ]

        AfterFetchTodos response ->
            { model | todos = response }
                ! [ Cmd.none ]

        AfterPutNewTodo oldId response ->
            case RemoteData.toMaybe response of
                Nothing ->
                    model ! [ Cmd.none ]

                Just newtodo ->
                    let
                        ( newtodos, cmd ) =
                            RemoteData.update amendId model.todos

                        amendId : List Todo -> ( List Todo, Cmd Msg )
                        amendId todos =
                            let
                                updateId todo =
                                    { todo | id = newtodo.id }
                            in
                                List.Extra.updateIf (\t -> t.id == oldId) updateId todos
                                    ! [ Cmd.none ]
                    in
                        { model | todos = newtodos } ! [ cmd ]



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
            , onClick (ToggleTodo todo.id)
            ]
            []
        , div [] [ text todo.value ]
        , button
            [ Style.form__btn
            , Style.flex__right
            , onClick (DelTodo todo.id)
            , disabled (not todo.done)
            ]
            [ text "Delete" ]
        ]


viewFooter : Html Msg
viewFooter =
    div [ Style.footer ]
        [ text "This is a nice footer." ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
