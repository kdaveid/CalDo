module Pages.Events.Id_ exposing (Model, Msg, page)

import Browser.Navigation exposing (Key, pushUrl)
import Data.Alarm exposing (Alarm, Trigger, stringToTrigger, triggerString, triggerToUiString)
import Data.ToDo exposing (Frequency(..), ToDo, emptyToDo, freqFromStr)
import Extras.Html exposing (block, viewLabel, viewLinkWithDetails, viewOrdinalFreqText)
import Gen.Params.Edit.Id_ exposing (Params)
import Gen.Route exposing (Route(..))
import Html exposing (Html, button, div, footer, h1, h3, header, input, label, option, p, section, select, text, textarea)
import Html.Attributes as HA exposing (attribute, checked, class, disabled, id, name, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Infra exposing (Session)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Request.Request exposing (deleteToDo, getNewToDo, getToDo, saveToDo)
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
    , error : Maybe String
    }


init : Maybe Session -> String -> ( Model, Cmd Msg )
init mbSession todoId =
    ( { todo = Loading, error = Nothing }
    , Maybe.map
        (\c ->
            getToDo c.origin todoId OnFetchDataComplete
        )
        mbSession
        |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = OnFetchDataComplete (WebData ToDo)


update : Maybe Session -> Key -> Msg -> Model -> ( Model, Cmd Msg )
update mbSession pageKey msg model =
    case msg of
        OnFetchDataComplete data ->
            ( { model | todo = data }, Cmd.none )



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


viewToDoOrError : Model -> Html Msg
viewToDoOrError model =
    case model.todo of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success todo ->
            div [ class "container is-max-widescreen" ]
                []

        RemoteData.Failure httpError ->
            viewError (httpErrorToString httpError)


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
