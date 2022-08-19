module Translation.Alarm exposing (..)

import Data.Alarm exposing (Trigger(..))


trigger : Trigger -> String
trigger t =
    case t of
        None ->
            "Kein"

        Minutes0 ->
            "Zum Ereigniszeitpunkt"

        Minutes15 ->
            "15 Minuten vorher"

        Minutes30 ->
            "30 Minuten vorher"

        Hours1 ->
            "1 Stunde vorher"

        Hours6 ->
            "6 Stunden vorher"

        Hours12 ->
            "12 Stunden vorher"

        Days1 ->
            "1 Tag vorher"

        Unknown ->
            "unbekannt"

