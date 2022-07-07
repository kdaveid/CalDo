module Pages.Home_ exposing (Model, Msg, page)

import Data.ToDo exposing (Freqency, ToDo, freqToStr)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (Html, button, dd, div, dl, dt, fieldset, h1, h2, h3, input, label, small, table, tbody, td, text, th, thead, tr)
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
    , toDos : Maybe (List ToDo)
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { message = "Nothing to see here "
      , error = Nothing
      , isFetching = False
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


view : Model -> View msg
view model =
    { title = "Homepage"
    , body =
        [ div [ class "container m-3" ]
            [ h1 [] [ text "Welcome to CalDo" ]
            , viewToDoList model.toDos
            , viewCreate
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


viewToDo : ToDo -> Html msg
viewToDo todo =
    div [] [ text todo.name ]


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


radioFormCheck : List (Html.Attribute msg) -> Html msg
radioFormCheck radioAttrs =
    let
        radioButtonAttributes : List (Html.Attribute msg)
        radioButtonAttributes =
            [ class "form-check-input" --position-static
            , type_ "radio"
            ]
    in
    formCheck
        [ input
            (radioButtonAttributes ++ radioAttrs)
            []
        ]


viewFreqRadio name val lbl =
    div [ class "form-check form-check-inline" ]
        [ input [ type_ "radio", class "form-check-input", HA.name name, value val ] []
        , label [ class "form-check-label", HA.for name ] [ text lbl ]
        ]


viewCreate : Html msg
viewCreate =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ text "Add new" ]
        , div
            [ class "card-body" ]
            [ div [ class "mb-3" ]
                [ viewLabel [ text "Name" ]
                , Html.input [ type_ "text", HA.name "name", class "form-control", HA.attribute "aria-describedby" "nameHelp" ] []
                , div [ class "form-text", HA.attribute "id" "nameHelp" ] [ text "Name des Kalender-Eintrags" ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Beschreibung" ]
                , Html.textarea [ HA.name "description", class "form-control" ] []
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Wiederholung" ]
                , div [ class "row g-3" ]
                    [ div [ class "col-auto" ]
                        [ viewFreqRadio "no-frequency" "none" "Nie"
                        , viewFreqRadio "second-frequency" "secondly" "Sekündlich"
                        , viewFreqRadio "minute-frequency" "minutly" "Minütlich"
                        , viewFreqRadio "hourly-frequency" "hourly" "Stündlich"
                        , viewFreqRadio "daily-frequency" "daily" "Täglich"
                        , viewFreqRadio "weekly-frequency" "weekly" "Wöchentlich"
                        , viewFreqRadio "monthly-frequency" "monthly" "Monatlich"
                        , viewFreqRadio "yearly-frequency" "yearly" "Jährlich"
                        ]
                    ]
                ]
            , div [ class "mb-3" ]
                [ viewLabel [ text "Intervall" ]
                , Html.input [ type_ "number", HA.name "interval", class "form-control", HA.attribute "rows" "3", HA.value "1" ] []
                ]
            , div [ class "mb-3" ]
                [ button [ type_ "submit", class "btn btn-primary" ] [ text "Absenden" ]
                ]
            ]
        ]
