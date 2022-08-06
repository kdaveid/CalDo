module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Browser.Dom exposing (Viewport)
import Browser.Events as E
import Infra exposing (Session, decodeSession)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (Value)
import Request exposing (Request)
import Result exposing (toMaybe)
import Task


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



-- Sub.none
