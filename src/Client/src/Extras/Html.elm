module Extras.Html exposing (viewLabel, viewLink)

import Gen.Route as Route exposing (Route)
import Html exposing (Html)
import Html.Attributes exposing (class, href)


viewLabel : List (Html msg) -> Html msg
viewLabel =
    Html.label [ class "form-label" ]


viewLink : String -> Route -> Html msg
viewLink label route =
    Html.a [ href (Route.toHref route) ] [ Html.text label ]
