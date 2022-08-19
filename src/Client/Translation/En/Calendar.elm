module Translation.Calendar exposing (..)

import Translation.Main

calendarUrl : String
calendarUrl = Translation.Main.calendar ++ " Url"

calendarUrlExplanation postfix = "You can subscribe to the calendar with this url" ++ postfix