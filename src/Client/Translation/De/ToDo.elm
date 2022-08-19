module Translation.ToDo exposing (..)

import Data.ToDo exposing (Frequency(..))

freqUntil : String
freqUntil = 
    " bis "

freqForever : String
freqForever = 
    ", für immer"


freqOnce : String
freqOnce =
    "Einmalig"


freqEvery : String
freqEvery =
    "Startet jeden "


freq : Frequency -> String
freq freqency =
    case freqency of
        None ->
            "Keine"

        Secondly ->
            "Sekündlich"

        Minutely ->
            "Minütlich"

        Hourly ->
            "Stündlich"

        Daily ->
            "Täglich"

        Weekly ->
            "Wöchentlich"

        Monthly ->
            "Monatlich"

        Yearly ->
            "Jährlich"

        Unknown ->
            "Unbekannt"
freqEveryLong : Frequency -> String
freqEveryLong frequency = 
        case frequency of
            None ->
                "N/A"

            Secondly ->
                "Sekunde"

            Minutely ->
                "Minute"

            Hourly ->
                "Stunde"

            Daily ->
                "Tag"

            Weekly ->
                "Woche"

            Monthly ->
                "Monat"

            Yearly ->
                "Jahr"

            Unknown ->
                "N/A"
