module Pages.Home_ exposing (Model, Msg, page)

import Data.ToDo exposing (Frequency(..), ToDo, emptyToDo)
import DatePicker exposing (ChangeEvent(..))
import Extras.Html exposing (dateToString, viewLink, viewOrdinalFreqText)
import Gen.Params.Calendar exposing (Params)
import Gen.Params.Events.Id_ exposing (Params)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (Html, dd, div, dl, dt, h1, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA exposing (checked, class, type_)
import Http
import Infra exposing (..)
import Page
import Request
import Request.Request exposing (getToDos)
import Request.Util exposing (httpErrorToString)
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update shared
        , view = view shared
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
    , reload shared.session
    )


type Msg
    = Loading
    | FetchData
    | OnFetchDataComplete (Result Http.Error (List ToDo))


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


reload : Maybe Session -> Cmd Msg
reload mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getToDos origin OnFetchDataComplete)
        |> Maybe.withDefault Cmd.none


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "CalDo"
    , body =
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text "CalDo" ]
                , p [ class "subtitle" ] [ text "The to do list with history, in your calendar" ]
                , viewErrorMessage model.error
                , viewToDoList shared.windowWidth model.toDos
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


viewToDoList : Int -> Maybe (List ToDo) -> Html msg
viewToDoList windowWidth mbTodos =
    case mbTodos of
        Just todos ->
            if List.length todos >= 1 then
                if windowWidth > 600 then
                    div [ class "card" ]
                        [ div [ class "card-header" ] [ p [ class "card-header-title" ] [ text "To Do List" ] ]
                        , div [ class "card-content" ]
                            [ viewTable todos ]
                        , div [ class "card-footer" ] [ viewAddLink, viewCalLink ]
                        ]

                else
                    div []
                        [ List.map (\t -> viewBox t) todos |> div []
                        , viewHelperBox
                        ]

            else
                viewInfo

        Nothing ->
            viewInfo


viewBox : ToDo -> Html msg
viewBox todo =
    div [ class "card mb-2" ]
        [ div [ class "card-header" ] [ p [ class "card-header-title" ] [ text todo.name ] ]
        , div [ class "card-content" ]
            [ div [ class "content" ]
                [ dl [] [ dt [] [ div [] [ text "Enabled: ", Html.input [ type_ "checkbox", checked todo.enabled, HA.disabled True ] [] ] ] ]
                , dl []
                    [ dt [] [ text "Frequency: " ]
                    , dd [] [ text (viewOrdinalFreqText todo.repetitionUntil todo.interval todo.frequency) ]
                    ]
                , dl []
                    [ dt [] [ text "Beginning: " ]
                    , dd [] [ dateToString todo.startDT |> text ]
                    ]
                ]
            ]
        , div [ class "card-footer" ]
            [ viewLink [ class "card-footer-item" ] "Edit" (Route.Edit__Id_ { id = todo.uid })
            , viewLink [ class "card-footer-item" ] "Events" (Route.Events__Id_ { id = todo.uid })
            ]
        ]


viewHelperBox : Html msg
viewHelperBox =
    div [ class "card m-5" ]
        [ div [ class "card-footer" ] [ viewAddLink, viewCalLink ]
        ]


viewTable : List ToDo -> Html msg
viewTable todos =
    table [ class "table is-fullwidth" ]
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Frequency" ]
                , th [] [ text "Beginning" ]
                , th [] [ text "Done ToDos" ]
                , th [] [ text "Enabled" ]
                ]
            ]
        , List.map (\s -> viewToDoTblRow s) todos |> tbody []
        ]


viewAddLink : Html msg
viewAddLink =
    viewLink
        [ class "card-footer-item" ]
        "Add ToDo"
        (Route.Edit__Id_ { id = "new" })


viewCalLink : Html msg
viewCalLink =
    viewLink
        [ class "card-footer-item" ]
        "Show Calendar (iCal)"
        Route.Calendar


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
        , td [] [ text (viewOrdinalFreqText todo.repetitionUntil todo.interval todo.frequency) ]
        , td [] [ dateToString todo.startDT |> text ]
        , td []
            [ Html.a [ HA.href (Route.toHref (Route.Events__Id_ { id = todo.uid })) ]
                [ Html.span [] [ text "Events" ]

                -- ,Html.span [ class "icon-text" ]
                --     [ Html.span [ class "icon" ]
                --         [ ionicon "add-circle-outline"
                --         ]
                --     ]
                ]
            ]
        , td [] [ Html.input [ type_ "checkbox", checked todo.enabled, HA.disabled True ] [] ]
        ]
