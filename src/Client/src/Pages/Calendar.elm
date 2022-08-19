module Pages.Calendar exposing (Model, Msg, page)

import Gen.Params.Calendar exposing (Params)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Infra exposing (Session)
import Page
import Request
import Request.Request exposing (getPlainTextCal, getPlainTextCalUrl)
import Request.Util exposing (httpErrorToString)
import Shared exposing (defaultBody)
import Translation.Calendar
import Translation.Main
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { cal : Maybe String
    , calUrl : Maybe String
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { cal = Nothing, calUrl = Nothing }, Cmd.batch [ loadCalendarText shared.session, loadCalendarUrl shared.session ] )


loadCalendarText : Maybe Session -> Cmd Msg
loadCalendarText mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getPlainTextCal origin OnGetCalendarComplete)
        |> Maybe.withDefault Cmd.none


loadCalendarUrl : Maybe Session -> Cmd Msg
loadCalendarUrl mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getPlainTextCalUrl origin OnGetCalendarUrlComplete)
        |> Maybe.withDefault Cmd.none



-- UPDATE


type Msg
    = OnGetCalendarComplete (Result Http.Error String)
    | OnGetCalendarUrlComplete (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGetCalendarComplete (Ok data) ->
            ( { model | cal = Just data }, Cmd.none )

        OnGetCalendarComplete (Err err) ->
            ( { model | cal = Just (httpErrorToString err) }, Cmd.none )

        OnGetCalendarUrlComplete (Ok data) ->
            ( { model | calUrl = Just data }, Cmd.none )

        OnGetCalendarUrlComplete (Err err) ->
            ( { model | calUrl = Just (httpErrorToString err) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "CalDo " ++ Translation.Main.calendar
    , body =
        [ defaultBody (Just "Calendar")
            [ viewCalendarUrl model
            , viewCalendar model
            ]
        ]
    }


viewCalendar : Model -> Html msg
viewCalendar model =
    Html.article [ class "message" ]
        [ div [ class "message-header" ]
            [ Html.p [] [ text Translation.Main.calendar ] ]
        , div [ class "message-body" ]
            [ case model.cal of
                Just calStr ->
                    Html.pre [] [ text calStr ]

                Nothing ->
                    div [] []
            ]
        ]


viewCalendarUrl : Model -> Html msg
viewCalendarUrl model =
    Html.article [ class "message" ]
        [ div [ class "message-header" ]
            [ Html.p []
                [ text Translation.Calendar.calendarUrl ]
            ]
        , div [ class "message-body" ]
            [ let
                urlText =
                    case model.calUrl of
                        Just url ->
                            url

                        Nothing ->
                            Translation.Main.error
              in
              div []
                [ div [ class "content" ] [ Translation.Calendar.calendarUrlExplanation ":" |> text ]
                , Html.pre [] [ text urlText ]
                ]
            ]
        ]
