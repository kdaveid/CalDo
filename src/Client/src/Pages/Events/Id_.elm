module Pages.Events.Id_ exposing (Model, Msg, page)

import Browser.Navigation exposing (Key)
import Data.ToDo exposing (Frequency(..), ToDo)
import Data.ToDoEvent exposing (ToDoEvent)
import Extras.Html exposing (dateToString, ionicon)
import Gen.Params.Edit.Id_ exposing (Params)
import Gen.Route exposing (Route(..))
import Html exposing (Html, button, div, h3, input, label, p, text, textarea)
import Html.Attributes as HA exposing (attribute, class, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (..)
import Infra exposing (Session)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Request.Request exposing (deleteEvent, getEventList, getNewEvent, getToDo, saveEvent)
import Request.Util exposing (httpErrorToString)
import Shared exposing (defaultBody)
import Translation.Events as Translation
import Translation.Main as MTranslation
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
    , newEvent : WebData ToDoEvent
    }


init : Maybe Session -> String -> ( Model, Cmd Msg )
init mbSession todoId =
    ( { todo = Loading, error = Nothing, events = Loading, newEvent = Loading }
    , Maybe.map
        (\c ->
            Cmd.batch
                [ getToDo c.origin todoId OnTodoDataComplete
                , getEventList c.origin todoId OnEventDataComplete
                , getNewEvent c.origin todoId OnNewEventDataComplete
                ]
        )
        mbSession
        |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = OnTodoDataComplete (WebData ToDo)
    | OnEventDataComplete (WebData (List ToDoEvent))
    | OnDeleteEvent Int
    | OnDeleteEventCompleted (Result Http.Error String)
    | OnNewEventDataComplete (WebData ToDoEvent)
    | OnNewEventDateChanged String
    | OnNewEventRemarksChanged String
    | OnNewEventAdjustCalendarChanged Bool
    | OnSaveNewEvent
    | OnSaveCompleted (WebData ToDoEvent)


update : Maybe Session -> Key -> Msg -> Model -> ( Model, Cmd Msg )
update mbSession _ msg model =
    case msg of
        OnTodoDataComplete data ->
            ( { model | todo = data }, Cmd.none )

        OnEventDataComplete data ->
            ( { model | events = data }, Cmd.none )

        OnDeleteEvent id ->
            ( model, delete mbSession id model.todo )

        OnDeleteEventCompleted (Ok _) ->
            ( model
            , case model.todo of
                RemoteData.Success todo ->
                    Maybe.map (\s -> getEventList s.origin todo.uid OnEventDataComplete) mbSession
                        |> Maybe.withDefault Cmd.none

                RemoteData.Failure _ ->
                    Cmd.none

                _ ->
                    Cmd.none
            )

        OnDeleteEventCompleted (Err _) ->
            ( model, Cmd.none )

        OnNewEventDataComplete data ->
            ( { model | newEvent = data }, Cmd.none )

        OnNewEventDateChanged newDate ->
            ( { model | newEvent = updateEvent (\d -> { d | date = newDate }) model.newEvent }, Cmd.none )

        OnNewEventRemarksChanged remarks ->
            ( { model | newEvent = updateEvent (\d -> { d | remarks = remarks }) model.newEvent }, Cmd.none )

        OnNewEventAdjustCalendarChanged val ->
            ( { model | newEvent = updateEvent (\d -> { d | adjustCalendar = val }) model.newEvent }, Cmd.none )

        OnSaveNewEvent ->
            ( model, saveWebDataEvent mbSession model.newEvent )

        OnSaveCompleted wdEvent ->
            ( model
            , case wdEvent of
                RemoteData.Success evt ->
                    Maybe.map
                        (\c ->
                            Cmd.batch
                                [ getEventList c.origin evt.todoUid OnEventDataComplete
                                , getNewEvent c.origin evt.todoUid OnNewEventDataComplete
                                ]
                        )
                        mbSession
                        |> Maybe.withDefault Cmd.none

                RemoteData.Failure _ ->
                    Cmd.none

                _ ->
                    Cmd.none
            )


updateEvent : (ToDoEvent -> ToDoEvent) -> WebData ToDoEvent -> WebData ToDoEvent
updateEvent fn evnt =
    RemoteData.map fn evnt


saveWebDataEvent : Maybe Session -> WebData ToDoEvent -> Cmd Msg
saveWebDataEvent mbSession wdEvent =
    case wdEvent of
        RemoteData.Success evt ->
            Maybe.map (\s -> saveEvent s.origin evt OnSaveCompleted) mbSession |> Maybe.withDefault Cmd.none

        RemoteData.Failure _ ->
            Cmd.none

        _ ->
            Cmd.none


delete : Maybe Session -> Int -> RemoteData Http.Error ToDo -> Cmd Msg
delete mbSession id wdTodo =
    case wdTodo of
        RemoteData.Success todo ->
            Maybe.map (\s -> deleteEvent s.origin todo id OnDeleteEventCompleted) mbSession
                |> Maybe.withDefault Cmd.none

        RemoteData.Failure _ ->
            Cmd.none

        _ ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ defaultBody (Just MTranslation.events)
            [ viewCreateEventForm model
            , viewPastEvents model.events
            ]
        ]
    }


viewPastEvents : WebData (List ToDoEvent) -> Html Msg
viewPastEvents wdEvents =
    div [ class "card" ]
        [ div [ class "card-header" ] [ div [ class "card-header-title" ] [ text Translation.pastEvents ] ]
        , div [ class "card-content" ]
            [ viewEventsOrError wdEvents
            ]
        ]


viewEventsOrError : WebData (List ToDoEvent) -> Html Msg
viewEventsOrError wdEvents =
    case wdEvents of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success events ->
            if List.length events > 0 then
                div [ class "container is-max-widescreen" ]
                    (List.map
                        (\e -> viewEvent e)
                        events
                    )

            else
                div [ class "box" ] [ text Translation.noEvents ]

        RemoteData.Failure httpError ->
            viewError (httpErrorToString httpError)


viewEvent : ToDoEvent -> Html Msg
viewEvent evt =
    div [ class "notification is-info mb-5" ]
        [ p []
            [ viewAdjustmentIcon evt
            , text (dateToString evt.date)
            , text " - "
            , viewRemarks evt.remarks
            ]
        , button [ class "delete", onClick (OnDeleteEvent evt.eventId) ] []
        ]


viewRemarks : String -> Html msg
viewRemarks remarks =
    let
        remText =
            if String.length remarks > 0 then
                remarks

            else
                "none"
    in
    text ("Remarks: " ++ remText)


viewAdjustmentIcon : ToDoEvent -> Html msg
viewAdjustmentIcon evt =
    if evt.adjustCalendar then
        Html.span [ class "icon-text" ]
            [ Html.span [ class "icon" ]
                [ ionicon "calendar-outline"
                ]
            ]

    else
        div [] []


viewError : String -> Html Msg
viewError errorMessage =
    div [ class "box" ]
        [ h3 [] [ text MTranslation.error ]
        , text (MTranslation.error ++ ": " ++ errorMessage)
        ]


viewCreateEventForm : Model -> Html Msg
viewCreateEventForm model =
    let
        todoName =
            case model.todo of
                RemoteData.Success todo ->
                    todo.name

                _ ->
                    "not found"
    in
    case model.newEvent of
        RemoteData.Success evt ->
            div [ class "box" ]
                [ p [] [ text Translation.toDoAsDone ]
                , Html.h4 [ class "title is-5" ] [ text todoName ]
                , div [ class "field" ]
                    [ label [ class "label" ]
                        [ text Translation.doneAt ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "date", value (String.left 10 evt.date), onInput OnNewEventDateChanged ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ]
                        [ text MTranslation.remarks ]
                    , div [ class "control" ]
                        [ textarea [ class "textarea", placeholder "Specialties", value evt.remarks, onInput OnNewEventRemarksChanged ]
                            []
                        ]
                    ]
                , div [ class "field" ]
                    [ input
                        [ class "is-checkradio"
                        , id "adjustCalendar"
                        , type_ "checkbox"
                        , name "adjustCalendar"
                        , HA.checked evt.adjustCalendar
                        ]
                        []
                    , label [ attribute "for" "adjustCalendar", onClick (OnNewEventAdjustCalendarChanged (not evt.adjustCalendar)) ] [ text Translation.adjustCalendarEnabled ]
                    ]
                , div [ class "field is-grouped" ]
                    [ div [ class "control" ]
                        [ button [ class "button is-link", onClick OnSaveNewEvent ]
                            [ text Translation.addEventAction ]
                        ]
                    ]
                ]

        _ ->
            div [] []
