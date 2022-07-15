module Pages.Home_ exposing (Model, Msg, page)

import Data.ToDo exposing (Frequency(..), ToDo, emptyToDo, freqToStr)
import DatePicker exposing (ChangeEvent(..))
import Extras.Html exposing (dateToString, viewLink, viewOrdinalFreqText)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, div, h1, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA exposing (checked, class, type_)
import Http
import Infra exposing (..)
import Page
import Request
import Request.Request exposing (getPlainTextCal, getToDos)
import Request.Util exposing (httpErrorToString)
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update shared
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { message : String
    , error : Maybe String
    , isFetching : Bool
    , current : ToDo
    , toDos : Maybe (List ToDo)
    , cal : Maybe String
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { message = "Nothing to see here "
      , error = Nothing
      , isFetching = False
      , current = emptyToDo
      , toDos = Nothing
      , cal = Nothing
      }
    , Cmd.batch [ reload shared.session, loadCalendarText shared.session ]
    )


type Msg
    = Loading
    | FetchData
    | OnFetchDataComplete (Result Http.Error (List ToDo))
    | OnGetCalendarComplete (Result Http.Error String)


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        Loading ->
            ( model, Cmd.none )

        FetchData ->
            ( { model | message = "Fetching... " }, Cmd.none )

        OnFetchDataComplete (Ok data) ->
            ( { model | isFetching = False, message = "Loaded", toDos = Just data }, Cmd.none )

        OnFetchDataComplete (Err err) ->
            ( { model | isFetching = False, error = Just (httpErrorToString err) }, Cmd.none )

        OnGetCalendarComplete (Ok data) ->
            ( { model | cal = Just data }, Cmd.none )

        OnGetCalendarComplete (Err err) ->
            ( { model | cal = Just (httpErrorToString err) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


reload : Maybe Session -> Cmd Msg
reload mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getToDos origin OnFetchDataComplete)
        |> Maybe.withDefault Cmd.none


loadCalendarText : Maybe Session -> Cmd Msg
loadCalendarText mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getPlainTextCal origin OnGetCalendarComplete)
        |> Maybe.withDefault Cmd.none


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "CalDo" ]
                , p [ class "subtitle" ] [ text "The to do list with history, in your calendar" ]
                , viewErrorMessage model.error
                , viewToDoList model.toDos
                ]
            ]
        ]
    }


viewErrorMessage : Maybe String -> Html msg
viewErrorMessage err =
    case err of
        Just str ->
            div [ class "alert alert-warning", HA.attribute "role" "alert" ] [ text str ]

        Nothing ->
            text ""


viewToDoList : Maybe (List ToDo) -> Html msg
viewToDoList mbTodos =
    case mbTodos of
        Just todos ->
            if List.length todos >= 1 then
                div [ class "card" ]
                    [ div [ class "card-header" ] [ p [ class "card-header-title" ] [ text "To Do List" ] ]
                    , div [ class "card-content" ]
                        [ table [ class "table" ]
                            [ thead []
                                [ tr []
                                    [ th [] [ text "Name" ]
                                    , th [] [ text "Start" ]
                                    , th [] [ text "Until" ]
                                    , th [] [ text "Enabled" ]
                                    ]
                                ]
                            , List.map (\s -> viewToDoTblRow s) todos |> tbody []
                            ]
                        ]
                    , div [ class "card-footer" ] [ viewAddLink ]
                    ]

            else
                viewInfo

        Nothing ->
            viewInfo


viewAddLink : Html msg
viewAddLink =
    viewLink
        [ class "card-footer-item" ]
        "Add one"
        (Route.Edit__Id_ { id = "new" })


viewInfo : Html msg
viewInfo =
    div [ class "notification" ]
        [ text "No ToDos found - "
        , viewLink [] "create one!" (Route.Edit__Id_ { id = "new" })
        ]


viewToDoTblRow : ToDo -> Html msg
viewToDoTblRow todo =
    tr []
        [ th [] [ viewLink [] todo.name (Route.Edit__Id_ { id = todo.uid }) ]

        -- , td [] [ text (freqToStr todo.frequency) ]
        -- , td [] [ text (String.fromInt todo.interval) ]
        , td [] [ dateToString todo.startDT |> text ]
        , td [] [ text (viewOrdinalFreqText todo.repetitionUntil todo.interval todo.frequency) ]
        , td [] [ Html.input [ type_ "checkbox", checked todo.enabled, HA.disabled True ] [] ]
        ]


viewRepOrDate : { a | repetitionUntilForEver : Bool, repetitionUntil : String } -> String
viewRepOrDate todo =
    if todo.repetitionUntilForEver then
        "forever"

    else
        "until " ++ String.left 10 todo.repetitionUntil
