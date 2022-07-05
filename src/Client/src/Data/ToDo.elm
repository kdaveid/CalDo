module Data.ToDo exposing (..)

import Json.Decode as JD
import Json.Decode.Pipeline as JD

type Freqency 
    =  None
    |  Secondly
    |  Minutely
    |  Hourly
    |  Daily
    |  Weekly
    |  Monthly
    |  Yearly
    |  Unknown

type alias ToDo =
    { uid : String
    , name : String
    , description : Maybe String
    , startDT : String
    , endDT : String
    , frequency : Freqency
    , enabled : Bool
    , interval : Int
    }

toDoDecoder : JD.Decoder ToDo
toDoDecoder =
    JD.succeed ToDo
        |> JD.required "uid" JD.string
        |> JD.required "name" JD.string
        |> JD.optional "description" (JD.nullable JD.string) Nothing
        |> JD.required "startDT" JD.string
        |> JD.required "endDT" JD.string
        |> JD.required "frequency" (JD.string |> JD.andThen frequencyDecoder)
        |> JD.required "enabled" JD.bool
        |> JD.required "interval" JD.int


frequencyDecoder : String -> JD.Decoder Freqency
frequencyDecoder str =
    case str |> String.toLower of
        "None" ->
            JD.succeed None

        "Secondly" ->
            JD.succeed Secondly

        "Minutely" ->
            JD.succeed Minutely

        "Hourly" ->
            JD.succeed Hourly

        "Daily" ->
            JD.succeed Daily

        "Weekly" ->
            JD.succeed Weekly

        "Monthly" ->
            JD.succeed Monthly

        "Yearly" ->
            JD.succeed Yearly

        _ ->
            JD.succeed Unknown
