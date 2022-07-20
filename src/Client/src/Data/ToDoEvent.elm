module Data.ToDoEvent exposing (ToDoEvent, eventDecoder, eventListDecoder)

import Json.Decode as JD
import Json.Decode.Pipeline as JD


type alias ToDoEvent =
    { eventId : Int
    , todoUid : String
    , date : String
    , remarks : String
    , adjustCalendar : Bool
    }


eventListDecoder : JD.Decoder (List ToDoEvent)
eventListDecoder =
    JD.list eventDecoder


eventDecoder : JD.Decoder ToDoEvent
eventDecoder =
    JD.succeed ToDoEvent
        |> JD.required "eventId" JD.int
        |> JD.required "calendarToDoId" JD.string
        |> JD.required "date" JD.string
        |> JD.required "remarks" JD.string
        |> JD.required "adjustCalendar" JD.bool
