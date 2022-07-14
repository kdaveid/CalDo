module Extras.Html exposing (block, dateToString, viewLabel, viewLink, viewLinkWithDetails, viewOrdinalFreqText)

import Data.ToDo exposing (Frequency(..))
import Date
import Gen.Route as Route exposing (Route)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, href)
import RemoteData exposing (RemoteData(..))
import Time


viewLabel : List (Html msg) -> Html msg
viewLabel =
    Html.label [ class "label" ]


viewLink : List (Attribute msg) -> String -> Route -> Html msg
viewLink attribs label route =
    let
        att =
            href (Route.toHref route) :: attribs
    in
    Html.a att [ Html.text label ]


viewLinkWithDetails : List (Attribute msg) -> List (Html msg) -> Route -> Html msg
viewLinkWithDetails attribs elem route =
    let
        att =
            href (Route.toHref route) :: attribs
    in
    Html.a att elem


block : List (Html msg) -> Html msg
block =
    Html.div [ class "block" ]


dateToString : String -> String
dateToString str =
    case Date.fromIsoString (String.left 10 str) of
        Ok val ->
            Date.format "dd.MM.yyyy" val

        Err err ->
            err



--format "MM/dd/yyyy" Time.utc (Time.millisToPosix 1575021804192)


viewOrdinalFreqText : String -> Int -> Frequency -> String
viewOrdinalFreqText repetitionUntil interval freq =
    let
        extra =
            if repetitionUntil == "" then
                ", forever"

            else
                " until " ++ dateToString repetitionUntil

        --String.left 10 repetitionUntil
    in
    if interval <= 0 then
        "Runs just once "

    else
        case freq of
            None ->
                "Runs just once"

            Secondly ->
                getFreqText interval "second" extra

            Minutely ->
                getFreqText interval "minute" extra

            Hourly ->
                getFreqText interval "hour" extra

            Daily ->
                getFreqText interval "day" extra

            Weekly ->
                getFreqText interval "week" extra

            Monthly ->
                getFreqText interval "month" extra

            Yearly ->
                getFreqText interval "year" extra

            Unknown ->
                "Unknown - an error!"


getFreqText : Int -> String -> String -> String
getFreqText int freq extra =
    if int == 1 then
        "Runs every " ++ freq ++ extra

    else
        "Runs every " ++ String.fromInt int ++ " " ++ freq ++ "s" ++ extra
