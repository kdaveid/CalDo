module Pages.Home_ exposing (Model, Msg, page)

import Data.ToDo exposing (Frequency(..), ToDo, emptyToDo, freqToStr)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (Html, button, dd, div, dl, dt, fieldset, h1, h2, h3, i, input, label, small, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA exposing (checked, class, style, type_, value)
import Http
import Infra exposing (..)
import Page
import Request
import Request.Request exposing (getToDos)
import Request.Util exposing (httpErrorToString)
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
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
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { message = "Nothing to see here "
      , error = Nothing
      , isFetching = False
      , current = emptyToDo
      , toDos = Nothing
      }
    , reload shared.session
    )


type Msg
    = Loading
    | FetchData
    | OnFetchDataComplete (Result Http.Error (List ToDo))


update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        Loading ->
            ( model, Cmd.none )

        FetchData ->
            ( { model | message = "Fetching... " }, Cmd.none )

        OnFetchDataComplete (Ok data) ->
            ( { model | isFetching = False, message = "Loaded", toDos = Just data }, Cmd.none )

        OnFetchDataComplete (Err err) ->
            ( { model | isFetching = False, message = httpErrorToString err }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


reload : Maybe Session -> Cmd Msg
reload mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getToDos origin OnFetchDataComplete)
        |> Maybe.withDefault Cmd.none



-- VIEW


formCheck : List (Html msg) -> Html msg
formCheck =
    div [ class "form-check" ]


formGroup : List (Html msg) -> Html msg
formGroup =
    div [ class "form-group" ]


row : List (Html msg) -> Html msg
row =
    div [ class "row" ]


card : List (Html msg) -> Html msg
card =
    div [ class "card" ]


view : Model -> View msg
view model =
    { title = "Homepage"
    , body =
        [ div [ class "container m-3" ]
            [ h1 [] [ text "Welcome to CalDo" ]
            , viewToDoList model.toDos
            , viewCreate model.current
            ]
        ]
    }


viewToDoList : Maybe (List ToDo) -> Html msg
viewToDoList mbTodos =
    case mbTodos of
        Just todos ->
            div []
                [ h3 [] [ text "To Do List" ]
                , table [ class "table" ]
                    [ thead []
                        [ tr []
                            [ th [ HA.scope "col" ] [ text "Name" ]
                            , th [ HA.scope "col" ] [ text "Frequency" ]
                            , th [ HA.scope "col" ] [ text "Interval" ]
                            , th [ HA.scope "col" ] [ text "Enabled" ]
                            ]
                        ]
                    , List.map (\s -> viewToDoTblRow s) todos |> tbody []
                    ]
                ]

        Nothing ->
            div [] [ text "No ToDos found - create one!" ]


viewLabel : List (Html msg) -> Html msg
viewLabel =
    Html.label [ class "form-label" ]


viewToDoTblRow : ToDo -> Html msg
viewToDoTblRow todo =
    tr []
        [ th [ HA.scope "row" ] [ text todo.name ]
        , td [] [ text (freqToStr todo.frequency) ]
        , td [] [ text (String.fromInt todo.interval) ]
        , td [] [ viewEnabled todo.enabled ]
        ]


viewEnabled : Bool -> Html msg
viewEnabled value =
    div [ class "form-check" ] [ Html.input [ type_ "checkbox", checked value, HA.disabled True ] [] ]



-- radioFormCheck : List (Html.Attribute msg) -> Html msg
-- radioFormCheck radioAttrs =
--     let
--         radioButtonAttributes : List (Html.Attribute msg)
--         radioButtonAttributes =
--             [ class "form-check-input"
--             , class "form-check-inline" --position-static
--             , type_ "radio"
--             ]
--     in
--     formCheck
--         [ input
--             (radioButtonAttributes ++ radioAttrs)
--             []
--         ]


viewFreqRadio : String -> String -> String -> Html msg
viewFreqRadio name val lbl =
    let
        defaultAttrib =
            [ type_ "radio", class "form-check-input", HA.id name, HA.name "frequency", value val ]
    in
    div [ class "form-check form-check-inline" ]
        [ input defaultAttrib []
        , label [ class "form-check-label", HA.for name ] [ text lbl ]
        ]


viewCreate : ToDo -> Html msg
viewCreate todo =
    card
        [ div [ class "card-header" ]
            [ text "Add new" ]
        , div
            [ class "card-body" ]
            [ div [ class "mb-3" ]
                [ viewLabel [ text "Name" ]
                , Html.input [ type_ "text", HA.name "name", class "form-control", HA.attribute "aria-describedby" "nameHelp" ] []
                , div [ class "form-text", HA.attribute "id" "nameHelp" ] [ text "Summary / Name of the calendar entry" ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Description" ]
                , Html.textarea [ HA.name "description", class "form-control", HA.attribute "aria-describedby" "descriptionHelp" ] []
                , div [ class "form-text", HA.attribute "id" "descriptionHelp" ] [ text "Calendar entry content" ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Repeated" ]
                , div [ class "row g-3" ]
                    [ div [ class "col-auto" ]
                        [ viewFreqRadio "never-frequency" "never" "Never"
                        , viewFreqRadio "second-frequency" "secondly" "Secondly"
                        , viewFreqRadio "minute-frequency" "minutly" "Minutly"
                        , viewFreqRadio "hourly-frequency" "hourly" "Hourly"
                        , viewFreqRadio "daily-frequency" "daily" "Daily"
                        , viewFreqRadio "weekly-frequency" "weekly" "Weekly"
                        , viewFreqRadio "monthly-frequency" "monthly" "Monthly"
                        , viewFreqRadio "yearly-frequency" "yearly" "Yearly"
                        ]
                    ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Interval" ]
                , Html.input [ type_ "number", HA.name "interval", class "form-control", HA.attribute "rows" "3", HA.value "1" ] []
                ]
            , div [ class "mb-3" ]
                [ viewOrdinalFreqText todo
                ]
            , div [ class "mb-3" ]
                [ button [ type_ "submit", class "btn btn-primary" ] [ text "Absenden" ]
                ]
            ]
        ]


viewOrdinalFreqText : ToDo -> Html msg
viewOrdinalFreqText todo =
    if todo.interval <= 0 then
        viewLabel [ text "Runs once" ]

    else
        case todo.frequency of
            Never ->
                viewAlert "Runs once"

            Secondly ->
                viewFreqLabel todo.interval "second"

            Minutely ->
                viewFreqLabel todo.interval "minute"

            Hourly ->
                viewFreqLabel todo.interval "hour"

            Daily ->
                viewFreqLabel todo.interval "day"

            Weekly ->
                viewFreqLabel todo.interval "week"

            Monthly ->
                viewFreqLabel todo.interval "month"

            Yearly ->
                viewFreqLabel todo.interval "year"

            Unknown ->
                viewAlert "Unknown - an error!"



-- _ ->
--     viewLabel [ text "error" ]


viewAlert : String -> Html msg
viewAlert str =
    div [ class "alert alert-primary", HA.attribute "role" "alert" ] [ text str ]


viewFreqLabel : Int -> String -> Html msg
viewFreqLabel int freq =
    if int == 1 then
        viewAlert ("Runs every " ++ freq)

    else
        viewAlert ("Runs every " ++ String.fromInt int ++ " " ++ freq ++ "s")
