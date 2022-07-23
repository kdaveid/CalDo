module Request.Request exposing (deleteEvent, deleteToDo, getEventList, getNewEvent, getNewToDo, getPlainTextCal, getToDo, getToDos, saveEvent, saveToDo)

import Data.ToDo exposing (ToDo, toDoDecoder, toDoEncoder)
import Data.ToDoEvent exposing (ToDoEvent, eventDecoder, eventEncoder, eventListDecoder)
import Http
import Json.Decode as JD
import RemoteData exposing (WebData)
import Request.Util exposing (..)
import Shared exposing (Msg)


getToDos : String -> (Result Http.Error (List ToDo) -> msg) -> Cmd msg
getToDos host msg =
    getJson
        (apiUrlArr host [ "todo" ])
        msg
        (JD.list toDoDecoder)


getToDo : String -> String -> (WebData ToDo -> msg) -> Cmd msg
getToDo host id msg =
    Http.get
        { url = apiUrlArr host [ "todo", id ]
        , expect =
            toDoDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


getNewToDo : String -> (WebData ToDo -> msg) -> Cmd msg
getNewToDo host msg =
    Http.get
        { url = apiUrlArr host [ "todo", "create" ]
        , expect =
            toDoDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


getPlainTextCal : String -> (Result Http.Error String -> msg) -> Cmd msg
getPlainTextCal host msg =
    Request.Util.getString
        (apiUrlArr host [ "calendar" ])
        msg


saveToDo : String -> ToDo -> (WebData ToDo -> msg) -> Cmd msg
saveToDo host todo msg =
    Http.post
        { url = apiUrlArr host [ "todo" ]
        , body = Http.jsonBody (toDoEncoder todo)
        , expect =
            toDoDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


saveEvent : String -> ToDoEvent -> (WebData ToDoEvent -> msg) -> Cmd msg
saveEvent host evt msg =
    Http.post
        { url = apiUrlArr host [ "events" ]
        , body = Http.jsonBody (eventEncoder evt)
        , expect =
            eventDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


deleteToDo : String -> ToDo -> (WebData ToDo -> msg) -> Cmd msg
deleteToDo host todo msg =
    Http.post
        { url = apiUrlArr host [ "todo", "delete" ]
        , body = Http.jsonBody (toDoEncoder todo)
        , expect =
            toDoDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


getEventList : String -> String -> (WebData (List ToDoEvent) -> msg) -> Cmd msg
getEventList host id msg =
    Http.get
        { url = apiUrlArr host [ "events", id ]
        , expect =
            eventListDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


getNewEvent : String -> String -> (WebData ToDoEvent -> msg) -> Cmd msg
getNewEvent host id msg =
    Http.get
        { url = apiUrlArr host [ "events", id, "new" ]
        , expect =
            eventDecoder
                |> Http.expectJson (RemoteData.fromResult >> msg)
        }


deleteEvent : String -> ToDo -> Int -> (Result Http.Error String -> msg) -> Cmd msg
deleteEvent host todo eventId msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrlArr host [ "events", todo.uid, String.fromInt eventId ]
        , body = Http.emptyBody
        , expect = Http.expectString msg
        , timeout = Nothing
        , tracker = Nothing
        }
