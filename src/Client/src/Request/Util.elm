module Request.Util exposing (apiUrl, apiUrlArr, getJson, getJsonTask, httpErrorToString, resolver)

import Http
import Json.Decode as JD
import Path
import Task exposing (Task)


apiUrl : String -> String -> String
apiUrl origin str =
    origin ++ Path.inApp ("/api" ++ str)



-- |> Debug.log "apiUrl"
-- apiUrl : String -> String
-- apiUrl path =
--     Path.inApp ("/api/" ++ path) |> Debug.log "apiUrl"


apiUrlArr : String -> List String -> String
apiUrlArr origin strs =
    origin ++ "/api/" ++ String.join "/" strs


getJson : String -> (Result Http.Error a -> msg) -> JD.Decoder a -> Cmd msg
getJson url msg decoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getJsonTask : String -> JD.Decoder a -> Task Http.Error a
getJsonTask url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = resolver decoder
        , timeout = Nothing
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus status ->
            "Unexpected Status: " ++ String.fromInt status

        Http.BadUrl err ->
            "Bad Url: " ++ err

        Http.BadBody str ->
            str


resolver : JD.Decoder a -> Http.Resolver Http.Error a
resolver decoder =
    Http.stringResolver
        (\resp ->
            case resp of
                Http.BadUrl_ str ->
                    Err (Http.BadUrl str)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    case JD.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (Http.BadBody (JD.errorToString err))
        )
