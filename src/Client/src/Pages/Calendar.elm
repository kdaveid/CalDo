module Pages.Calendar exposing (Model, Msg, page)

import Gen.Params.Calendar exposing (Params)
import Html exposing (Html, div, h1, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes as HA exposing (checked, class, type_)
import Http
import Infra exposing (Session)
import Page
import Request
import Request.Request exposing (getPlainTextCal)
import Request.Util exposing (httpErrorToString)
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { cal : Maybe String }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { cal = Nothing }, loadCalendarText shared.session )


loadCalendarText : Maybe Session -> Cmd Msg
loadCalendarText mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getPlainTextCal origin OnGetCalendarComplete)
        |> Maybe.withDefault Cmd.none



-- UPDATE


type Msg
    = OnGetCalendarComplete (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGetCalendarComplete (Ok data) ->
            ( { model | cal = Just data }, Cmd.none )

        OnGetCalendarComplete (Err err) ->
            ( { model | cal = Just (httpErrorToString err) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "CalDo Calendar"
    , body =
        [ div [ class "section " ]
            [ viewCalendar model.cal
            ]
        ]
    }


viewCalendar : Maybe String -> Html msg
viewCalendar mbCalString =
    case mbCalString of
        Just calStr ->
            let
                _ =
                    Debug.log "cal" calStr
            in
            Html.article [ class "message" ]
                [ div [ class "message-header" ] [ Html.p [] [ text "Calendar" ] ]
                , div [ class "message-body" ] [ Html.pre [] [ text calStr ] ]
                ]

        Nothing ->
            div [] []
