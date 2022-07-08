module Pages.Home_ exposing (Model, Msg, page)

import Data.ToDo exposing (Frequency(..), ToDo, emptyToDo, freqFromStr, freqToStr)
import Element exposing (html)
import Element.Font exposing (hairline)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (Html, button, dd, div, dl, dt, fieldset, h1, h2, h3, i, input, label, p, small, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA exposing (checked, class, style, type_, value)
import Html.Events exposing (onCheck, onInput)
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
    | OnNameChange String
    | OnDescriptionChange String
    | OnIntervalChange String
    | OnFrequencyChange String


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
            ( { model | isFetching = False, error = Just (httpErrorToString err) }, Cmd.none )

        OnNameChange newName ->
            let
                current =
                    model.current
            in
            ( { model | current = { current | name = newName } }, Cmd.none )

        OnDescriptionChange descr ->
            let
                current =
                    model.current
            in
            ( { model | current = { current | description = Just descr } }, Cmd.none )

        OnIntervalChange val ->
            let
                current =
                    model.current
            in
            ( { model | current = { current | interval = val |> String.toInt |> Maybe.withDefault 0 } }, Cmd.none )

        OnFrequencyChange val ->
            let
                current =
                    model.current
            in
            ( { model | current = { current | frequency = val |> freqFromStr } }, Cmd.none )


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


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ div [ class "container m-3" ]
            [ h1 [] [ text "Welcome to CalDo" ]
            , p [ class "lead" ] [ text "Calender-ToDo (CalDo) - the to do list with history in your calendar." ]
            , viewErrorMessage model.error
            , viewToDoList model.toDos
            , viewEdit model.current
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
            div [ class "card mb-3" ]
                [ div [ class "card-header" ] [ text "To Do List" ]
                , div [ class "card-body" ]
                    [ table [ class "table" ]
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
                ]

        Nothing ->
            div [ class "alert alert-info", HA.attribute "role" "alert" ] [ text "No ToDos found - create one!" ]


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


viewFreqRadio : String -> String -> String -> Bool -> Html Msg
viewFreqRadio name val lbl checked =
    let
        checkedAttrib =
            if checked then
                [ HA.attribute "checked" "checked" ]

            else
                []

        defaultAttrib =
            [ type_ "radio", class "form-check-input", HA.id name, HA.name "frequency", value val ]
    in
    div [ class "form-check form-check-inline" ]
        [ input (defaultAttrib ++ onInput OnFrequencyChange :: checkedAttrib) []
        , label [ class "form-check-label", HA.for name ] [ text lbl ]
        ]


viewEdit : ToDo -> Html Msg
viewEdit todo =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ if todo.uid == "" then
                text "Add new"

              else
                text "Edit"
            ]
        , div
            [ class "card-body" ]
            [ div [ class "mb-3" ]
                [ viewLabel [ text "Name" ]
                , Html.input [ type_ "text", HA.name "name", HA.placeholder "Clean the washmaschine", class "form-control", HA.attribute "aria-describedby" "nameHelp", onInput OnNameChange ] []
                , div [ class "form-text", HA.attribute "id" "nameHelp" ] [ text "Summary / Name of the calendar entry" ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Description" ]
                , Html.textarea [ HA.name "description", HA.placeholder "- Clean surfaces with soap\n- Run with 90°C\n- Wipe it dry", class "form-control", HA.attribute "rows" "4", HA.attribute "aria-describedby" "descriptionHelp", onInput OnDescriptionChange ] []
                , div [ class "form-text", HA.attribute "id" "descriptionHelp" ] [ text "Calendar entry content" ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12 mb-3" ]
                    [ viewLabel [ text "Start" ]
                    , Html.input [ HA.name "start", class "form-control" ] []
                    ]
                , div [ class "col-md-6 mb-3" ]
                    [ viewLabel [ text "End" ]
                    , Html.input [ HA.name "end", class "form-control" ] []
                    ]
                , div [ class "col-md-6 mb-3" ]
                    [ Html.label [ class "form-label" ] [ text "" ]
                    , div [ class "form-check mt-3" ]
                        [ Html.input [ HA.id "noEnd", type_ "checkbox", class "form-check-input" ] []
                        , Html.label [ class "form-check-label", HA.attribute "for" "noEnd" ] [ text "Open End" ]
                        ]
                    ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Alert" ]
                , Html.select [ HA.name "alert", class "form-select" ]
                    [ Html.option [ value "none" ] [ text "None" ]
                    , Html.option [ value "30minutes" ] [ text "30 mintutes" ]
                    , Html.option [ value "1hour" ] [ text "1 hour" ]
                    , Html.option [ value "4hours" ] [ text "4 hours" ]
                    , Html.option [ value "12hours" ] [ text "12 hours" ]
                    , Html.option [ value "1day" ] [ text "1 day" ]
                    ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Repetition" ]
                , div [ class "row g-3" ]
                    [ div [ class "col-auto" ]
                        [ viewFreqRadio "none-frequency" "none" "Never" (todo.frequency == Data.ToDo.None)
                        , viewFreqRadio "second-frequency" "secondly" "Secondly" (todo.frequency == Data.ToDo.Secondly)
                        , viewFreqRadio "minute-frequency" "minutely" "Minutely" (todo.frequency == Data.ToDo.Minutely)
                        , viewFreqRadio "hourly-frequency" "hourly" "Hourly" (todo.frequency == Data.ToDo.Hourly)
                        , viewFreqRadio "daily-frequency" "daily" "Daily" (todo.frequency == Data.ToDo.Daily)
                        , viewFreqRadio "weekly-frequency" "weekly" "Weekly" (todo.frequency == Data.ToDo.Weekly)
                        , viewFreqRadio "monthly-frequency" "monthly" "Monthly" (todo.frequency == Data.ToDo.Monthly)
                        , viewFreqRadio "yearly-frequency" "yearly" "Yearly" (todo.frequency == Data.ToDo.Yearly)
                        ]
                    ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Interval" ]
                , Html.input [ type_ "number", HA.name "interval", class "form-control", HA.attribute "rows" "3", HA.value (todo.interval |> String.fromInt), onInput OnIntervalChange ] []
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
        viewLabel [ text "Runs just once" ]

    else
        case todo.frequency of
            None ->
                viewAlert "Runs just once"

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


formCheck : List (Html msg) -> Html msg
formCheck =
    div [ class "form-check" ]


formGroup : List (Html msg) -> Html msg
formGroup =
    div [ class "form-group" ]


row : List (Html msg) -> Html msg
row =
    div [ class "row" ]
