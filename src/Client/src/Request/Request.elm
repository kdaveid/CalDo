module Request.Request exposing (getToDos)

import Data.ToDo exposing (ToDo)
import Http
import Json.Decode as JD
import RemoteData exposing (WebData)
import Request.Util exposing (..)
import Data.ToDo exposing (toDoDecoder)


getToDos : String -> (Result Http.Error (List ToDo) -> msg) -> Cmd msg
getToDos host msg =
    getJson
        (apiUrlArr host
            [ "todo"
            ]
        )
        msg
        (JD.list toDoDecoder)


