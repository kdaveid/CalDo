module Shared exposing
    ( Flags
    , Model
    , Msg
    , defaultBody
    , footer
    , init
    , subscriptions
    , update
    )

import Browser.Dom exposing (Viewport)
import Browser.Events as E
import Extras.Html exposing (ionicon, viewLinkWithDetails)
import Gen.Route
import Html exposing (Html, a, div, h1, li, nav, p, text, ul)
import Html.Attributes as HA exposing (attribute, class, href)
import Infra exposing (Session, decodeSession)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Request exposing (Request)
import Result exposing (toMaybe)
import Task
import Translation.Main as Translation


type alias Flags =
    Value


type alias Model =
    { session : Maybe Session
    , windowWidth : Int
    }


type Msg
    = NoOp
    | GotNewWidth Int
    | GetInitialViewport Viewport


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { session = decodeValue decodeSession flags |> toMaybe, windowWidth = 1000 }, Task.perform GetInitialViewport Browser.Dom.getViewport )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotNewWidth val ->
            ( { model | windowWidth = val }, Cmd.none )

        GetInitialViewport vp ->
            ( { model | windowWidth = vp.viewport.width |> round }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    E.onResize (\w _ -> GotNewWidth w)


defaultBody : Maybe String -> List (Html msg) -> Html msg
defaultBody breadCrumb viewElements =
    let
        elements =
            [ Html.article [ class "media mb-5" ]
                [ div [ class "media-left" ] [ p [ class "image is-48x48" ] [ Html.img [ attribute "src" "/assets/calDo-icon.png" ] [] ] ]
                , div [ class "media-content" ]
                    [ h1 [ class "title" ] [ text "CalDo" ]
                    , p [ class "subtitle" ] [ text Translation.headerText ]
                    ]
                ]
            , viewBreadCrumb breadCrumb
            ]

        all =
            viewElements ++ [ footer ]
    in
    div [ class "section" ]
        [ List.append elements all
            |> div [ class "container" ]
        ]


viewBreadCrumb : Maybe String -> Html msg
viewBreadCrumb mbBreadCrumb =
    case mbBreadCrumb of
        Just breadCrumb ->
            nav [ class "breadcrumb", attribute "aria-label" "breadcrumbs" ]
                [ ul []
                    [ li []
                        [ viewLinkWithDetails [] [ text "Home" ] Gen.Route.Home_
                        ]
                    , li [ class "is-active" ]
                        [ a [ href "#", attribute "aria-current" "page" ]
                            [ text breadCrumb ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []


footer : Html msg
footer =
    Html.footer [ class "footer has-text-grey-light" ]
        [ div [ class "content" ]
            [ Html.nav [ class "level is-mobile" ]
                [ div
                    [ class "level-item has-text-centered" ]
                    [ div []
                        [ p [ class "heading" ] [ text "Fork me on" ]
                        , div [ class "title" ] [ Html.a [ HA.href "https://github.com/kdaveid/Caldo", HA.target "_blank" ] [ ionicon "logo-github" ] ]
                        ]
                    ]
                , div
                    [ class "level-item has-text-centered" ]
                    [ div []
                        [ p [ class "heading" ] [ text "Powered by" ]
                        , div [ class "title" ] [ Html.a [ HA.href "https://elm-lang.org", HA.target "_blank" ] [ text "Elm" ] ]
                        ]
                    ]
                , div
                    [ class "level-item has-text-centered" ]
                    [ div []
                        [ p [ class "heading" ] [ text "Created with" ]
                        , div [ class "title" ] [ Html.a [ HA.href "https://bulma.io", HA.target "_blank" ] [ text "Bulma.io" ] ]
                        ]
                    ]
                ]
            , div [ class "has-text-centered" ]
                [ p []
                    [ Html.strong [] [ text "CalDo" ]
                    , text Translation.made
                    , Html.a [ HA.href "http://github.com/kdaveid", HA.target "_blank" ] [ text "David E. Keller" ]
                    , text Translation.codeLic
                    , Html.a [ HA.href "https://opensource.org/licenses/mit-license.php", HA.target "_blank" ] [ text "MIT" ]
                    , text Translation.contentCopyRight
                    ]
                ]
            ]
        ]
