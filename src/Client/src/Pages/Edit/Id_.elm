module Pages.Edit.Id_ exposing (Model, Msg, page)

import Browser.Navigation exposing (Key, pushUrl)
import Data.Alarm exposing (Alarm, Trigger, stringToTrigger, triggerString, triggerToUiString)
import Data.ToDo exposing (Frequency(..), ToDo, freqFromStr)
import Extras.Html exposing (block, viewLabel, viewLinkWithDetails, viewOrdinalFreqText)
import Gen.Params.Edit.Id_ exposing (Params)
import Gen.Route exposing (Route(..))
import Html exposing (Html, a, button, div, footer, h3, header, input, label, li, nav, option, p, section, select, text, textarea, ul)
import Html.Attributes as HA exposing (attribute, checked, class, disabled, href, id, name, selected, type_, value)
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
    , viewDeleteModal : Bool
    }


init : Maybe Session -> String -> ( Model, Cmd Msg )
init mbSession todoId =
    ( { todo = Loading, error = Nothing, viewDeleteModal = False }
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
    | OnEnabledChanged Bool
    | OnAlarmTriggerChanged String
    | OnRepetitionUntilDateChanged String
    | OnRepetitionForEver Bool
    | OnRepetitionUntilDate Bool
    | OnSave
    | OnSaveComplete (WebData ToDo)
    | OnDeleteModal Bool
    | OnDelete
    | OnDeleteComplete (WebData ToDo)


updateToDo : (ToDo -> ToDo) -> WebData ToDo -> WebData ToDo
updateToDo fn todo =
    RemoteData.map fn todo


setTrigger : String -> Alarm -> Alarm
setTrigger triggerStr alarm =
    { alarm | trigger = stringToTrigger triggerStr }


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

        OnEnabledChanged val ->
            ( { model | todo = updateToDo (\d -> { d | enabled = val }) model.todo }, Cmd.none )

        OnAlarmTriggerChanged val ->
            ( { model | todo = updateToDo (\d -> { d | alarm = d.alarm |> setTrigger val }) model.todo }, Cmd.none )

        OnStartChanged val ->
            ( { model | todo = updateToDo (\d -> { d | startDT = val }) model.todo }, Cmd.none )

        OnEndChanged val ->
            ( { model | todo = updateToDo (\d -> { d | endDT = val }) model.todo }, Cmd.none )

        OnRepetitionUntilDateChanged val ->
            ( { model | todo = updateToDo (\d -> { d | repetitionUntil = val }) model.todo }, Cmd.none )

        OnRepetitionForEver _ ->
            ( { model | todo = updateToDo (\d -> { d | repetitionUntilForEver = True, repetitionUntil = "" }) model.todo }, Cmd.none )

        OnRepetitionUntilDate _ ->
            ( { model | todo = updateToDo (\d -> { d | repetitionUntilForEver = False }) model.todo }, Cmd.none )

        OnSave ->
            ( model, model.todo |> RemoteData.map (save mbSession) |> RemoteData.withDefault Cmd.none )

        OnSaveComplete data ->
            ( { model | todo = data }, RemoteData.map (\_ -> pushUrl pageKey "/") data |> RemoteData.withDefault Cmd.none )

        OnDeleteModal show ->
            ( { model | viewDeleteModal = show }, Cmd.none )

        OnDelete ->
            ( model, model.todo |> RemoteData.map (delete mbSession) |> RemoteData.withDefault Cmd.none )

        OnDeleteComplete data ->
            ( model, RemoteData.map (\_ -> pushUrl pageKey "/") data |> RemoteData.withDefault Cmd.none )


save : Maybe Session -> ToDo -> Cmd Msg
save mbSession todo =
    Maybe.map (\s -> saveToDo s.origin todo OnSaveComplete) mbSession |> Maybe.withDefault Cmd.none


delete : Maybe Session -> ToDo -> Cmd Msg
delete mbSession todo =
    Maybe.map (\s -> deleteToDo s.origin todo OnDeleteComplete) mbSession |> Maybe.withDefault Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Edit"
    , body =
        [ div [ class "section" ]
            [ div [ class "container" ]
                [ Html.h2 [ class "title" ] [ text "Edit ToDo" ]
                , viewBreadCrumbs model
                , viewToDoOrError model
                ]
            ]
        ]
    }


viewToDoOrError : Model -> Html Msg
viewToDoOrError model =
    case model.todo of
        RemoteData.NotAsked ->
            text "Not asked"

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success todo ->
            div [ class "container is-max-widescreen" ]
                [ viewEdit todo
                , renderModal model
                ]

        RemoteData.Failure httpError ->
            viewError (httpErrorToString httpError)


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


viewDeleteModal : Model -> Html Msg
viewDeleteModal model =
    case model.todo of
        RemoteData.Success todo ->
            if model.viewDeleteModal then
                div
                    [ class "modal" ]
                    [ div
                        [ class "modal-dialog" ]
                        [ div [ class "modal-content" ]
                            [ div [ class "modal-header" ]
                                [ div [ class "modal-title" ] [ Html.h5 [] [ text ("Delete " ++ todo.name) ] ]
                                ]
                            , div [ class "modal-body" ]
                                [ Html.p [] [ text "Do you really want to delete it?" ]
                                ]
                            , div [ class "modal-footer" ]
                                [ button [ type_ "button", class "btn btn-secondary", attribute "data-bs-dismiss" "modal" ] [ text "Close" ]
                                , button [ type_ "button", class "btn btn-danger", onClick OnDelete ] [ text "Delete" ]
                                ]
                            ]
                        ]
                    ]

            else
                div [] []

        _ ->
            div [] []


renderModal : Model -> Html Msg
renderModal model =
    case model.todo of
        RemoteData.Success todo ->
            if model.viewDeleteModal then
                div [ class "modal is-active", attribute "aria-label" "Modal title" ]
                    [ div [ class "modal-background", onClick (OnDeleteModal False) ]
                        []
                    , div [ class "modal-card" ]
                        [ header [ class "modal-card-head" ]
                            [ p [ class "modal-card-title" ]
                                [ text ("Delete " ++ todo.name) ]
                            , button [ class "delete", onClick OnDelete, attribute "aria-label" "close" ]
                                []
                            ]
                        , section [ class "modal-card-body" ]
                            [ text "Modal contents" ]
                        , footer [ class "modal-card-foot" ]
                            [ button [ type_ "button", class "btn is-secondary is-active", onClick (OnDeleteModal False) ] [ text "Cancel" ]
                            , button [ type_ "button", class "btn is-danger", onClick OnDelete ] [ text "Delete" ]
                            ]
                        ]
                    ]

            else
                div [] []

        _ ->
            div [] []


viewEdit : ToDo -> Html Msg
viewEdit todo =
    div [ class "section" ]
        [ div [ class "card" ]
            [ div [ class "card-header" ]
                [ p [ class "card-header-title" ] [ text "Edit" ] ]
            , div
                [ class "card-content" ]
                [ viewNameAndEnable todo
                , viewDescription todo
                , viewStartEnd todo
                , viewAlarm todo
                , viewRepetition todo
                , viewInterval todo
                , viewRepetitionUntil todo
                , viewAlert (viewOrdinalFreqText todo.repetitionUntil todo.interval todo.frequency)
                , viewButtons
                ]
            ]
        ]


viewAlert : String -> Html msg
viewAlert str =
    div [ class "notification is-info is-light" ] [ text str ]


viewDescription : { a | description : String } -> Html Msg
viewDescription todo =
    block
        [ viewLabel [ text "Description" ]
        , div [ class "control" ]
            [ textarea
                [ HA.name "description"
                , HA.placeholder "- Clean surfaces with soap\n- Run with 90°C\n- Wipe it dry"
                , class "textarea"
                , attribute "rows" "4"
                , attribute "aria-describedby" "descriptionHelp"
                , value todo.description
                , onInput OnDescriptionChange
                ]
                []
            ]
        , p [ class "help" ] [ text "Calendar entry content" ]
        ]


viewNameAndEnable : ToDo -> Html Msg
viewNameAndEnable todo =
    div [ class "columns" ]
        [ div [ class "column is-two-thirds" ]
            [ viewLabel [ text "Name" ]
            , div [ class "control" ]
                [ input [ type_ "text", class "input", HA.placeholder "Clean the washmaschine", onInput OnNameChange, value todo.name ] []
                ]
            , p [ class "help" ] [ text "Summary / Name of the calendar entry" ]
            ]
        , div [ class "column" ]
            [ viewLabel [ text "Enable" ]
            , div [ class "field" ]
                [ input
                    [ class "is-checkradio"
                    , id "enabled"
                    , type_ "checkbox"
                    , name "enabledCheckbox"
                    , checked todo.enabled
                    ]
                    []
                , label [ attribute "for" "enabledCheckbox", onClick (OnEnabledChanged (not todo.enabled)) ] [ text "Enabled" ]
                ]
            ]
        ]


viewInterval : ToDo -> Html Msg
viewInterval todo =
    block
        [ div [ class "control" ]
            [ viewLabel [ text "Interval" ]
            , input [ type_ "number", class "input", HA.name "interval", HA.value (todo.interval |> String.fromInt), onInput OnIntervalChange ] []
            ]
        ]


viewRepetition : ToDo -> Html Msg
viewRepetition todo =
    block
        [ viewLabel [ text "Repetition" ]
        , div [ class "control" ]
            [ viewFreqRadio "none" "Never" (todo.frequency == Data.ToDo.None)
            , viewFreqRadio "secondly" "Secondly" (todo.frequency == Data.ToDo.Secondly)
            , viewFreqRadio "minutely" "Minutely" (todo.frequency == Data.ToDo.Minutely)
            , viewFreqRadio "hourly" "Hourly" (todo.frequency == Data.ToDo.Hourly)
            , viewFreqRadio "daily" "Daily" (todo.frequency == Data.ToDo.Daily)
            , viewFreqRadio "weekly" "Weekly" (todo.frequency == Data.ToDo.Weekly)
            , viewFreqRadio "monthly" "Monthly" (todo.frequency == Data.ToDo.Monthly)
            , viewFreqRadio "yearly" "Yearly" (todo.frequency == Data.ToDo.Yearly)
            ]
        ]


viewRepetitionUntil : ToDo -> Html Msg
viewRepetitionUntil todo =
    block
        [ viewLabel [ text "Until" ]
        , div [ class "control" ]
            [ div [ class "columns" ]
                [ div [ class "column" ]
                    [ div [ class "field" ]
                        [ input [ type_ "radio", class "is-checkradio is-info is-light", name "repetitionUntil", HA.id "forEverCheckbox", checked todo.repetitionUntilForEver, onCheck OnRepetitionForEver ] []
                        , label [ HA.for "forEverCheckbox" ] [ text " For ever" ]
                        ]
                    ]
                , div [ class "column" ]
                    [ div
                        [ class "field" ]
                        [ input [ type_ "radio", class "is-checkradio is-info is-light", name "repetitionUntil", HA.id "forDate", checked (not todo.repetitionUntilForEver), onCheck OnRepetitionUntilDate ] []
                        , label [ HA.for "forDate" ] [ text " Date" ]
                        ]
                    ]
                , div [ class "column is-half" ]
                    [ input
                        [ type_ "date"
                        , class "input"
                        , HA.name "untilDate"
                        , value (String.left 10 todo.repetitionUntil)
                        , disabled todo.repetitionUntilForEver
                        , onInput OnRepetitionUntilDateChanged
                        ]
                        []
                    ]
                ]
            ]
        ]


viewAlarm : ToDo -> Html Msg
viewAlarm todo =
    block
        [ div [ class "control" ]
            [ viewLabel [ text "Alarm" ]
            , div [ class "select" ]
                [ select [ onInput OnAlarmTriggerChanged ]
                    [ viewTriggerOption (todo.alarm.trigger == Data.Alarm.None) Data.Alarm.None
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Minutes0) Data.Alarm.Minutes0
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Minutes15) Data.Alarm.Minutes15
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Minutes30) Data.Alarm.Minutes30
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Hours1) Data.Alarm.Hours1
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Hours6) Data.Alarm.Hours6
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Hours12) Data.Alarm.Hours12
                    , viewTriggerOption (todo.alarm.trigger == Data.Alarm.Days1) Data.Alarm.Days1
                    ]
                ]
            ]
        ]


viewTriggerOption : Bool -> Trigger -> Html msg
viewTriggerOption selected opt =
    option [ value (opt |> triggerString), HA.selected selected ] [ text (opt |> triggerToUiString) ]


viewStartEnd : { a | startDT : String, endDT : String } -> Html Msg
viewStartEnd todo =
    block
        [ div [ class "columns" ]
            [ div [ class "column" ]
                [ viewLabel [ text "Start" ]
                , div [ class "control" ]
                    [ input
                        [ type_ "datetime-local"
                        , class "input"
                        , value (subStrDate todo.startDT)
                        , onInput OnStartChanged
                        ]
                        []
                    ]
                ]
            , div [ class "column" ]
                [ viewLabel [ text "End" ]
                , div [ class "control" ]
                    [ input
                        [ type_ "datetime-local"
                        , class "input"
                        , value (subStrDate todo.endDT)
                        , onInput OnEndChanged
                        ]
                        []
                    ]
                ]
            ]
        ]


viewButtons : Html Msg
viewButtons =
    div [ class "columns" ]
        [ div [ class "column" ]
            [ div [ class "buttons" ]
                [ button [ type_ "submit", class "button is-primary", onClick OnSave ] [ text "Save" ]
                , viewLinkWithDetails [ type_ "button", class "button is-light" ]
                    [ Html.span [] [ text "Cancel" ] ]
                    Gen.Route.Home_
                ]
            ]
        , div [ class "column" ] [ button [ type_ "button", class "button is-danger is-outlined is-pulled-right", onClick (OnDeleteModal True) ] [ text "Delete" ] ]
        ]


subStrDate : String -> String
subStrDate dt =
    String.left 16 dt


viewFreqRadio : String -> String -> Bool -> Html Msg
viewFreqRadio val lbl checked =
    let
        checkedAttrib =
            if checked then
                [ attribute "checked" "checked" ]

            else
                []

        defaultAttrib =
            [ type_ "radio", HA.name "frequency", value val, onInput OnFrequencyChange ]
    in
    label [ class "radio" ]
        [ input (defaultAttrib ++ checkedAttrib) []
        , text (" " ++ lbl ++ "  ")
        ]
