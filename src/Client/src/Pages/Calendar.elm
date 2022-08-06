module Pages.Calendar exposing (Model, Msg, page)

import Element exposing (html)
import Extras.Html exposing (viewLinkWithDetails)
import Gen.Params.Calendar exposing (Params)
import Gen.Route
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, type_)
import Http
import Infra exposing (Session)
import Page
import Request
import Request.Request exposing (getPlainTextCal, getPlainTextCalUrl)
import Request.Util exposing (httpErrorToString)
import Shared
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
    { title = "CalDo Calendar"
    , body =
        [ div [ class "section " ]
            [ viewCalendar model ]
        ]
    }


viewCalendar : Model -> Html msg
viewCalendar model =
    Html.article [ class "message" ]
        [ div [ class "message-header" ]
            [ Html.p [] [ text "Calendar" ]
            , viewLinkWithDetails [ type_ "button", class "button is-light" ]
                [ Html.span [] [ text "Home" ] ]
                Gen.Route.Home_
            ]
        , div [ class "message-body" ]
            [ case model.calUrl of
                Just url ->
                    div []
                        [ Html.h3 [] [ text "Calendar-Url" ]
                        , div [ class "content" ] [ text "You can subscribe to the calendar with this url: " ]
                        , Html.pre [] [ text url ]
                        ]

                Nothing ->
                    div [] []
            , Html.h3 [] [ text "Calendar" ]
            , case model.cal of
                Just calStr ->
                    Html.pre [] [ text calStr ]

                Nothing ->
                    div [] []
            ]
        ]
