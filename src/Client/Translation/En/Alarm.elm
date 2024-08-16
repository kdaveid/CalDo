module Translation.Alarm exposing (..)

import Data.ToDo exposing (Frequency(..))


freq : Frequency -> String
freq freqency =
    case freqency of
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
