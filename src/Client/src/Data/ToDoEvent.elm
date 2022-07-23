module Data.ToDoEvent exposing (ToDoEvent, eventDecoder, eventEncoder, eventListDecoder)

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE


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


eventEncoder : ToDoEvent -> JE.Value
eventEncoder evt =
    JE.object
        [ ( "eventId", JE.int evt.eventId )
        , ( "calendarToDoId", JE.string evt.todoUid )
        , ( "date", JE.string evt.date )
        , ( "remarks", JE.string evt.remarks )
        , ( "adjustCalendar", JE.bool evt.adjustCalendar )
        ]
