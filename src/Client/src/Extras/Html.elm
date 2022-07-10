module Extras.Html exposing (block, viewLabel, viewLink)

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


block : List (Html msg) -> Html msg
block =
    Html.div [ class "block" ]
