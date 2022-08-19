module Extras.Html exposing (block, dateToString, ionicon, viewLabel, viewLink, viewLinkWithDetails, viewOrdinalFreqText)

import Data.ToDo exposing (Frequency(..))
import Date
import Gen.Route as Route exposing (Route)
import Html exposing (Attribute, Html)
import Html.Attributes as Attr exposing (class, href)
import RemoteData exposing (RemoteData(..))
import Translation.ToDo as Translation


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
                Translation.freqForever

            else
                Translation.freqUntil ++ dateToString repetitionUntil

        --String.left 10 repetitionUntil
    in
    if interval <= 0 then
        Translation.freqOnce

    else
        case freq of
            None ->
                Translation.freqOnce

            Secondly ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Minutely ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Hourly ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Daily ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Weekly ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Monthly ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Yearly ->
                getFreqText interval (Translation.freqEveryLong freq) extra

            Unknown ->
                "Unknown - an error!"


getFreqText : Int -> String -> String -> String
getFreqText int freq extra =
    if int == 1 then
        Translation.freqEvery ++ freq ++ extra

    else
        Translation.freqEvery ++ String.fromInt int ++ " " ++ freq ++ "s" ++ extra


ionicon : String -> Html msg
ionicon iconName =
    Html.node "ion-icon" [ Attr.name iconName ] []



-- stylesheet : Html msg
-- stylesheet =
--     Html.node "link"
--         [ Attr.rel "stylesheet"
--         , Attr.href "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
--         ]
--         []
