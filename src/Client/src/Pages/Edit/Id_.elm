module Pages.Edit.Id_ exposing (Model, Msg, page)

import Browser.Navigation exposing (Key, pushUrl)
import Data.ToDo exposing (Frequency(..), ToDo, freqFromStr)
import Extras.Html exposing (viewLabel)
import Gen.Params.Edit.Id_ exposing (Params)
import Html exposing (Html, button, div, h3, input, label, text)
import Html.Attributes as HA exposing (checked, class, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Infra exposing (Session)
import Page
import RemoteData exposing (RemoteData(..), WebData)
import Request
import Request.Request exposing (getNewToDo, getToDo, saveToDo)
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
            if todoId == "new" then
                getNewToDo c.origin OnFetchDataComplete

            else
                getToDo c.origin todoId OnFetchDataComplete
        )
        mbSession
        |> Maybe.withDefault Cmd.none
    )



-- UPDATE


type Msg
    = OnFetchDataComplete (WebData ToDo)
    | OnNameChange String
    | OnDescriptionChange String
    | OnIntervalChange String
    | OnFrequencyChange String
    | OnStartChanged String
    | OnEndChanged String
    | OnSave
    | OnSaveComplete (WebData ToDo)


updateToDo : (ToDo -> ToDo) -> WebData ToDo -> WebData ToDo
updateToDo fn todo =
    RemoteData.map fn todo


update : Maybe Session -> Key -> Msg -> Model -> ( Model, Cmd Msg )
update mbSession pageKey msg model =
    case msg of
        OnFetchDataComplete data ->
            ( { model | todo = data }, Cmd.none )

        OnNameChange newName ->
            ( { model | todo = updateToDo (\d -> { d | name = newName }) model.todo }, Cmd.none )

        OnDescriptionChange descr ->
            ( { model | todo = updateToDo (\d -> { d | description = descr }) model.todo }, Cmd.none )

        OnIntervalChange val ->
            ( { model | todo = updateToDo (\d -> { d | interval = val |> String.toInt |> Maybe.withDefault 0 }) model.todo }, Cmd.none )

        OnFrequencyChange val ->
            ( { model | todo = updateToDo (\d -> { d | frequency = val |> freqFromStr }) model.todo }, Cmd.none )

        OnStartChanged val ->
            ( { model | todo = updateToDo (\d -> { d | startDT = val }) model.todo }, Cmd.none )

        OnEndChanged val ->
            ( { model | todo = updateToDo (\d -> { d | endDT = val }) model.todo }, Cmd.none )

        OnSave ->
            ( model, model.todo |> RemoteData.map (save mbSession) |> RemoteData.withDefault Cmd.none )

        OnSaveComplete data ->
            ( { model | todo = data }, RemoteData.map (\s -> pushUrl pageKey "/") data |> RemoteData.withDefault Cmd.none )


save : Maybe Session -> ToDo -> Cmd Msg
save mbSession todo =
    Maybe.map (\s -> saveToDo s.origin todo OnSaveComplete) mbSession |> Maybe.withDefault Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Edit", body = [ viewToDoOrError model ] }


viewToDoOrError : Model -> Html Msg
viewToDoOrError model =
    case model.todo of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success todo ->
            viewEdit todo

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


viewEdit : ToDo -> Html Msg
viewEdit todo =
    div [ class "container" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ if todo.name == "new" then
                    text "Add new"

                  else
                    text "Edit"
                ]
            , div
                [ class "card-body" ]
                [ div [ class "mb-3" ]
                    [ viewLabel [ text "Name" ]
                    , Html.input [ type_ "text", HA.name "name", HA.placeholder "Clean the washmaschine", class "form-control", HA.attribute "aria-describedby" "nameHelp", onInput OnNameChange, value todo.name ] []
                    , div [ class "form-text", HA.attribute "id" "nameHelp" ] [ text "Summary / Name of the calendar entry" ]
                    ]
                , div [ class "mb-3" ]
                    [ viewLabel [ text "Description" ]
                    , Html.textarea
                        [ HA.name "description"
                        , HA.placeholder "- Clean surfaces with soap\n- Run with 90°C\n- Wipe it dry"
                        , class "form-control"
                        , HA.attribute "rows" "4"
                        , HA.attribute "aria-describedby" "descriptionHelp"
                        , value todo.description
                        , onInput OnDescriptionChange
                        ]
                        []
                    , div [ class "form-text", HA.attribute "id" "descriptionHelp" ] [ text "Calendar entry content" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col-md-12 mb-3" ]
                        [ viewLabel [ text "Start" ]
                        , Html.input
                            [ type_ "datetime-local"
                            , HA.name "start"
                            , class "form-control"
                            , value (subStrDate todo.startDT)
                            , onInput OnStartChanged
                            ]
                            []
                        ]
                    , div [ class "col-md-6 mb-3" ]
                        [ viewLabel [ text "End" ]
                        , Html.input
                            [ type_ "datetime-local"
                            , HA.name "end"
                            , class "form-control"
                            , value (subStrDate todo.endDT)
                            , onInput OnEndChanged
                            ]
                            []
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
                    [ viewLabel [ text "Alarm" ]
                    , Html.select [ HA.name "alarm", class "form-select" ]
                        [ Html.option [ value "N" ] [ text "None" ]
                        , Html.option [ value "0M" ] [ text "0 mintutes" ]
                        , Html.option [ value "30M" ] [ text "30 mintutes" ]
                        , Html.option [ value "1H" ] [ text "1 hour" ]
                        , Html.option [ value "4H" ] [ text "4 hours" ]
                        , Html.option [ value "12H" ] [ text "12 hours" ]
                        , Html.option [ value "1D" ] [ text "1 day" ]
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
                    [ button [ type_ "submit", class "btn btn-primary", onClick OnSave ] [ text "Save" ]
                    ]
                ]
            ]
        ]


subStrDate : String -> String
subStrDate dt =
    String.left 19 dt



--2018-06-12T19:30


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
