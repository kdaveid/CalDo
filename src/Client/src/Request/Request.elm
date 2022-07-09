module Request.Request exposing (getPlainTextCal, getToDo, getToDos, saveToDo)

import Data.ToDo exposing (ToDo, toDoDecoder, toDoEncoder)
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
