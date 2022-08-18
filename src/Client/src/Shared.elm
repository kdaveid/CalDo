module Shared exposing
    ( Flags
    , Model
    , Msg
    , defaultBody
    , init
    , subscriptions
    , update
    )

import Browser.Dom exposing (Viewport)
import Browser.Events as E
import Extras.Html exposing (viewLinkWithDetails)
import Gen.Route
import Html exposing (Html, a, div, h1, li, nav, p, text, ul)
import Html.Attributes exposing (attribute, class, href)
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
    in
    div [ class "section" ]
        [ List.append elements viewElements |> div [ class "container" ]
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
