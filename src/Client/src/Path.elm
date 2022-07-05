module Path exposing (inApp, normalizeUrl)

import Url exposing (Url)


basePath : String
basePath =
    "/"


deployPath : String
deployPath =
    if String.endsWith "/" basePath then
        String.dropRight 1 basePath

    else
        basePath


inApp : String -> String
inApp path =
    deployPath ++ path


normalizeUrl : Url -> Url
normalizeUrl url =
    let
        newPath =
            if String.startsWith deployPath url.path then
                String.dropLeft (String.length deployPath) url.path

            else
                url.path
    in
    { url | path = newPath }
