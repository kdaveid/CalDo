module Data.ToDo exposing (Freqency, ToDo, toDoDecoder, frequencyDecoder, freqToStr)

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
        "none" ->
            JD.succeed None

        "secondly" ->
            JD.succeed Secondly

        "minutely" ->
            JD.succeed Minutely

        "hourly" ->
            JD.succeed Hourly

        "daily" ->
            JD.succeed Daily

        "weekly" ->
            JD.succeed Weekly

        "monthly" ->
            JD.succeed Monthly

        "yearly" ->
            JD.succeed Yearly

        _ ->
            JD.succeed Unknown

freqToStr : Freqency -> String
freqToStr freq =
    case freq of
       None ->
         "None"
       Secondly ->
         "Secondly"
       Minutely ->
         "Minutely"
       Hourly ->
         "Hourly"
       Daily ->
         "Daily"
       Weekly ->
         "Weekly"
       Monthly ->
         "Monthly"
       Yearly ->
         "Yearly"
       Unknown ->
         "Unknown"
            
