module Extras.Html exposing (block, viewLabel, viewLink, viewLinkWithDetails)

import Gen.Route as Route exposing (Route)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, href)


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
