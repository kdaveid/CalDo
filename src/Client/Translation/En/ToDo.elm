module Translation.ToDo exposing (..)

import Data.ToDo exposing (Frequency(..))



freqUntil : String
freqUntil = 
    " until "

freqForever : String
freqForever = 
    ", forever"


freqOnce : String
freqOnce = 
    "Runs just once"

freqEvery : String
freqEvery = 
    "Runs every "

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



freqEveryLong : Frequency -> String
freqEveryLong frequency =
    case frequency of
        None ->
            "N/A"

        Secondly ->
            "second"

        Minutely ->
            "minute"

        Hourly ->
            "hour"

        Daily ->
            "day"

        Weekly ->
            "week"

        Monthly ->
            "month"

        Yearly ->
            "year"

        Unknown ->
            "N/A"
