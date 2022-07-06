module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    )

import Infra exposing (Session, decodeSession)
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode exposing (Value)
import Request exposing (Request)
import Result exposing (toMaybe)


type alias Flags =
    Value




type alias Model =
    { session : Maybe Session }



type Msg
    = NoOp


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { session = decodeValue decodeSession flags |> toMaybe }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
