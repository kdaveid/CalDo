module Pages.Events.Id_ exposing (Model, Msg, page)

import Browser.Navigation exposing (Key, pushUrl)
import Data.Alarm exposing (Alarm, Trigger, stringToTrigger, triggerString, triggerToUiString)
import Data.ToDo exposing (Frequency(..), ToDo, emptyToDo, freqFromStr)
import Data.ToDoEvent exposing (..)
import Extras.Html exposing (block, ionicon, viewLabel, viewLinkWithDetails, viewOrdinalFreqText)
import Gen.Params.Edit.Id_ exposing (Params)
import Gen.Route exposing (Route(..))
import Html exposing (Html, button, div, footer, h1, h3, header, input, label, option, p, section, select, text, textarea)
import Html.Attributes as HA exposing (attribute, checked, class, disabled, id, name, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Infra exposing (Session)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Request.Request exposing (deleteToDo, getEventList, getNewToDo, getToDo, saveToDo)
import Request.Util exposing (httpErrorToString)
import Shared
import Task
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared.session req.params.id
        , update = update shared.session req.key
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { todo : WebData ToDo
    , events : WebData (List ToDoEvent)
    , error : Maybe String
    }


init : Maybe Session -> String -> ( Model, Cmd Msg )
init mbSession todoId =
    ( { todo = Loading, error = Nothing, events = Loading }
    , Maybe.map
        (\c ->
            Cmd.batch
                [ getToDo c.origin todoId OnTodoDataComplete
                , getEventList c.origin todoId OnEventDataComplete
                ]
        )
        mbSession
        |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = OnTodoDataComplete (WebData ToDo)
    | OnEventDataComplete (WebData (List ToDoEvent))


update : Maybe Session -> Key -> Msg -> Model -> ( Model, Cmd Msg )
update mbSession pageKey msg model =
    case msg of
        OnTodoDataComplete data ->
            ( { model | todo = data }, Cmd.none )

        OnEventDataComplete data ->
            ( { model | events = data }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ Html.h2 [ class "title" ] [ text "Events" ]
                , viewTodoOrError model
                , viewEventsOrError model
                ]
            ]
        ]
    }


viewTodoOrError : Model -> Html Msg
viewTodoOrError model =
    case model.todo of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            p [ class "subtitle" ] [ text "Loading..." ]

        RemoteData.Success todo ->
            p [ class "subtitle" ] [ text ("Todo: " ++ todo.name) ]

        RemoteData.Failure httpError ->
            viewError (httpErrorToString httpError)


viewEventsOrError : Model -> Html Msg
viewEventsOrError model =
    case model.events of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success events ->
            div [ class "container is-max-widescreen" ]
                (List.map
                    (\e -> viewEvent e)
                    events
                )

        RemoteData.Failure httpError ->
            viewError (httpErrorToString httpError)


viewEvent : ToDoEvent -> Html msg
viewEvent evt =
    div [ class "box" ]
        [ Html.h4 [] [ text evt.date, viewAdjustmentIcon evt ]
        , p [] [ text ("Remarks: " ++ evt.remarks) ]
        ]


viewAdjustmentIcon evt =
    if evt.adjustCalendar then
        Html.span [ class "icon-text is-pulled-right" ]
            [ Html.span [ class "icon" ]
                [ ionicon "calendar-outline"
                ]
            ]

    else
        div [] []


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]



-- viewTodo
