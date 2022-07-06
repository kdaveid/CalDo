module Infra exposing (Session, decodeSession)

import Json.Decode as JD
import Json.Decode.Pipeline as JD


type alias Session =
    { origin : String
    }


decodeSession : JD.Decoder Session
decodeSession =
    JD.succeed Session
        |> JD.required "origin" JD.string
