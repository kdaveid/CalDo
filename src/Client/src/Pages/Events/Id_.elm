module Pages.Events.Id_ exposing (Model, Msg, page)

import Browser.Navigation exposing (Key)
import Data.ToDo exposing (Frequency(..), ToDo)
import Data.ToDoEvent exposing (ToDoEvent)
import Extras.Html exposing (ionicon, viewLinkWithDetails)
import Gen.Params.Edit.Id_ exposing (Params)
import Gen.Route exposing (Route(..))
import Html exposing (Html, a, button, div, h3, input, label, li, nav, p, text, textarea, ul)
import Html.Attributes exposing (attribute, class, href, id, placeholder, type_)
import Html.Events exposing (onClick)
import Http exposing (..)
import Infra exposing (Session)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Request.Request exposing (deleteEvent, getEventList, getToDo)
import Request.Util exposing (httpErrorToString)
import Shared
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
    | OnDeleteEvent Int
    | OnDeleteEventCompleted (Result Http.Error String)


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
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ Html.h2 [ class "title" ] [ text "Events" ]
                , viewBreadCrumbs model
                , viewCreateEventForm model
                , viewEventsOrError model
                ]
            ]
        ]
    }


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


viewEvent : ToDoEvent -> Html Msg
viewEvent evt =
    Html.article [ class "message is-info" ]
        [ div [ class "message-header" ]
            [ viewAdjustmentIcon evt
            , text evt.date
            , button [ class "delete", onClick (OnDeleteEvent evt.eventId) ] []
            ]
        , div [ class "message-body" ] [ viewRemarks evt.remarks ]
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
    p [] [ text ("Remarks: " ++ remText) ]


viewAdjustmentIcon : a -> Html msg
viewAdjustmentIcon _ =
    Html.span [ class "icon-text" ]
        [ Html.span [ class "icon" ]
            [ ionicon "calendar-outline"
            ]
        ]


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


viewCreateEventForm : Model -> Html msg
viewCreateEventForm _ =
    div [ class "box" ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Date" ]
            , div [ class "control" ]
                [ input [ class "input", type_ "date" ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Remarks" ]
            , div [ class "control" ]
                [ textarea [ class "textarea", placeholder "Specialties" ]
                    []
                ]
            ]
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button [ class "button is-link" ]
                    [ text "Add event" ]
                ]
            , div [ class "control" ]
                [ button [ class "button is-link is-light" ]
                    [ text "Cancel" ]
                ]
            ]
        ]


viewBreadCrumbs : Model -> Html Msg
viewBreadCrumbs model =
    case model.todo of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Success todo ->
            nav [ class "breadcrumb", attribute "aria-label" "breadcrumbs" ]
                [ ul []
                    [ li []
                        [ viewLinkWithDetails [] [ text "Home" ] Gen.Route.Home_
                        ]
                    , li [ class "is-active" ]
                        [ a [ href "#", attribute "aria-current" "page" ]
                            [ text todo.name ]
                        ]
                    ]
                ]

        RemoteData.Loading ->
            p [ class "subtitle" ] [ text "Loading..." ]

        RemoteData.Failure httpError ->
            viewError (httpErrorToString httpError)
