module Request.Request exposing (deleteToDo, getEventList, getNewToDo, getPlainTextCal, getToDo, getToDos, saveToDo)

import Data.ToDo exposing (ToDo, toDoDecoder, toDoEncoder)
import Data.ToDoEvent exposing (ToDoEvent, eventListDecoder)
import Http
import Json.Decode as JD
import RemoteData exposing (WebData)
import Request.Util exposing (..)


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
