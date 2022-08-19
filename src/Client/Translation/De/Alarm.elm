module Translation.Alarm exposing (..)

import Data.ToDo exposing (Frequency(..))


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
