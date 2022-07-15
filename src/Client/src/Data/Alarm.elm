module Data.Alarm exposing (Alarm, Trigger(..), alarmDecoder, alarmEncoder, defaultAlarm, triggerString, triggerToUiString)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


type alias Alarm =
    { summary : String
    , description : String
    , trigger : Trigger
    }


type Trigger
    = None
    | Minutes0
    | Minutes30
    | Hours1
    | Hours6
    | Hours12
    | Days1
    | Unknown


defaultAlarm : Alarm
defaultAlarm =
    { summary = "Reminder", description = "Reminder", trigger = None }


alarmDecoder : JD.Decoder Alarm
alarmDecoder =
    JD.succeed Alarm
        |> JD.required "summary" JD.string
        |> JD.required "description" JD.string
        |> JD.required "trigger" (JD.string |> JD.andThen triggerDecoder)


alarmEncoder : Alarm -> JE.Value
alarmEncoder a =
    JE.object
        [ ( "summary", JE.string a.summary )
        , ( "description", JE.string a.description )
        , ( "trigger", JE.string (triggerString a.trigger) )
        ]


triggerDecoder : String -> JD.Decoder Trigger
triggerDecoder str =
    case str |> String.toLower of
        "PT0M" ->
            JD.succeed Minutes0

        "PT-30M" ->
            JD.succeed Minutes30

        "PT-1H" ->
            JD.succeed Hours1

        "PT-6H" ->
            JD.succeed Hours6

        "PT-12H" ->
            JD.succeed Hours12

        "PT-1D" ->
            JD.succeed Days1

        _ ->
            JD.succeed Unknown


triggerString : Trigger -> String
triggerString t =
    case t of
        None ->
            ""

        Minutes0 ->
            "PT0M"

        Minutes30 ->
            "PT-30M"

        Hours1 ->
            "PT-1H"

        Hours6 ->
            "PT-6H"

        Hours12 ->
            "PT-12H"

        Days1 ->
            "PT-1D"

        Unknown ->
            ""


triggerToUiString : Trigger -> String
triggerToUiString t =
    case t of
        None ->
            "None"

        Minutes0 ->
            "0 mintutes"

        Minutes30 ->
            "30 mintutes"

        Hours1 ->
            "1 hour"

        Hours6 ->
            "6 hours"

        Hours12 ->
            "12 hours"

        Days1 ->
            "1 day"

        Unknown ->
            "unknown"
