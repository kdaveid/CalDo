module Translation.Calendar exposing (..)

import Translation.Main


calendarUrl : String
calendarUrl =
    Translation.Main.calendar ++ "-Url"


calendarUrlExplanation postfix =
    "Du kannst mit dieser Url deinen Kalender abonnieren" ++ postfix
