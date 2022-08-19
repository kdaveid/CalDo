module Translation.Alarm exposing (..)

import Data.Alarm exposing (Trigger(..))


trigger : Trigger -> String
trigger t =
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