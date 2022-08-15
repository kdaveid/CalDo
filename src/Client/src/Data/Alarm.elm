module Data.Alarm exposing (Alarm, Trigger(..), alarmDecoder, alarmEncoder, defaultAlarm, stringToTrigger, triggerString, triggerToUiString)

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
    | Minutes15
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


stringToTrigger : String -> Trigger
stringToTrigger str =
    case str |> String.toUpper of
        "PT0M" ->
            Minutes0

        "-PT15M" ->
            Minutes15

        "-PT30M" ->
            Minutes30

        "-PT1H" ->
            Hours1

        "-PT6H" ->
            Hours6

        "-PT12H" ->
            Hours12

        "-PT1D" ->
            Days1

        _ ->
            Unknown


triggerDecoder : String -> JD.Decoder Trigger
triggerDecoder str =
    case str |> String.toUpper of
        "PT0M" ->
            JD.succeed Minutes0

        "-PT15M" ->
            JD.succeed Minutes15

        "-PT30M" ->
            JD.succeed Minutes30

        "-PT1H" ->
            JD.succeed Hours1

        "-PT6H" ->
            JD.succeed Hours6

        "-PT12H" ->
            JD.succeed Hours12

        "-PT1D" ->
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

        Minutes15 ->
            "-PT15M"

        Minutes30 ->
            "-PT30M"

        Hours1 ->
            "-PT1H"

        Hours6 ->
            "-PT6H"

        Hours12 ->
            "-PT12H"

        Days1 ->
            "-PT1D"

        Unknown ->
            ""


triggerToUiString : Trigger -> String
triggerToUiString t =
    case t of
        None ->
            "None"

        Minutes0 ->
            "At time of event"

        Minutes15 ->
            "15 minutes before"

        Minutes30 ->
            "30 minutes before"

        Hours1 ->
            "1 hour before"

        Hours6 ->
            "6 hours before"

        Hours12 ->
            "12 hours before"

        Days1 ->
            "1 day before"

        Unknown ->
            "unknown"
