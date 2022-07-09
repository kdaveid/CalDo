module Data.ToDo exposing (DateEditModel, Frequency(..), ToDo, emptyToDo, freqFromStr, freqToStr, frequencyDecoder, toDoDecoder, toDoEncoder)

import Date exposing (Date)
import DatePicker exposing (ChangeEvent(..))
import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


type Frequency
    = None
    | Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
    | Unknown


type alias DateEditModel =
    { pickerModel : DatePicker.Model
    , dateText : String
    , date : Maybe Date
    }


type alias ToDo =
    { uid : String
    , name : String
    , description : String
    , startDT : String
    , endDT : String
    , startEdit : DateEditModel
    , endEdit : DateEditModel
    , frequency : Frequency
    , enabled : Bool
    , interval : Int
    }


emptyToDo : ToDo
emptyToDo =
    { uid = ""
    , name = ""
    , description = ""
    , startDT = ""
    , endDT = ""
    , startEdit = { pickerModel = DatePicker.init, date = Nothing, dateText = "" }
    , endEdit = { pickerModel = DatePicker.init, date = Nothing, dateText = "" }
    , frequency = Monthly
    , enabled = False
    , interval = 1
    }


toDoDecoder : JD.Decoder ToDo
toDoDecoder =
    JD.succeed ToDo
        |> JD.required "uid" JD.string
        |> JD.required "name" JD.string
        |> JD.optional "description" JD.string ""
        |> JD.required "startDT" JD.string
        |> JD.required "endDT" JD.string
        |> JD.hardcoded { pickerModel = DatePicker.init, date = Nothing, dateText = "" }
        |> JD.hardcoded { pickerModel = DatePicker.init, date = Nothing, dateText = "" }
        |> JD.required "frequency" (JD.string |> JD.andThen frequencyDecoder)
        |> JD.required "enabled" JD.bool
        |> JD.required "interval" JD.int


toDoEncoder : ToDo -> JE.Value
toDoEncoder todo =
    JE.object
        [ ( "uid", JE.string todo.uid )
        , ( "name", JE.string todo.name )
        , ( "description", JE.string todo.description )
        , ( "startDT", JE.string todo.startDT )
        , ( "endDT", JE.string todo.endDT )
        , ( "frequency", JE.string (freqToStr todo.frequency) )
        , ( "enabled", JE.bool todo.enabled )
        , ( "interval", JE.int todo.interval )
        ]


frequencyDecoder : String -> JD.Decoder Frequency
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


freqToStr : Frequency -> String
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


freqFromStr : String -> Frequency
freqFromStr freq =
    case freq |> String.toLower of
        "none" ->
            None

        "secondly" ->
            Secondly

        "minutely" ->
            Minutely

        "hourly" ->
            Hourly

        "daily" ->
            Daily

        "weekly" ->
            Weekly

        "monthly" ->
            Monthly

        "yearly" ->
            Yearly

        _ ->
            Unknown
