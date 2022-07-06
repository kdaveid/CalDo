module Pages.Home_ exposing (Model, Msg, page)

import Http
import View exposing (View)
import Data.ToDo exposing (ToDo)
import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Page
import Request
import Infra exposing (..)
import Request.Request exposing (getToDos)
import Request.Util exposing (httpErrorToString)
import Shared
import Html exposing (Html, button, h1, dd, div, dl, dt, small, text)

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update shared
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { message : String
    , error : Maybe String
    , isFetching : Bool
    , toDos : Maybe (List ToDo)
    }


init : Shared.Model -> ( Model, Cmd Msg )
init shared =
    ( { message = "Nothing to see here "
      , error = Nothing
      , isFetching = False
      , toDos = Nothing
      }
    , reload shared.session
    )


type Msg
    = Loading
    | FetchData
    | OnFetchDataComplete (Result Http.Error (List ToDo))


update :  Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
update shared msg model =
    case msg of
        Loading ->
            ( model, Cmd.none )

        FetchData ->
            ( { model | message = "Fetching... " }, Cmd.none )

        OnFetchDataComplete (Ok data) ->
            ( { model | isFetching = False, message = "Loaded", toDos = Just data }, Cmd.none )

        OnFetchDataComplete (Err err) ->
            ( { model | isFetching = False, message = httpErrorToString err }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

reload : Maybe Session -> Cmd Msg
reload mbSession =
    mbSession
        |> Maybe.map .origin
        |> Maybe.map (\origin -> getToDos origin OnFetchDataComplete)
        |> Maybe.withDefault Cmd.none


view : Model -> View msg
view model =
    { title = "Homepage"
    , body = [ 
         div [ ] --class "container m-3" 
         [ h1 [] [ text "Welcome to CalDo"] 
         , viewToDoList model.toDos
        ]
        ]
    }

viewToDoList :  Maybe (List ToDo) -> Html msg
viewToDoList mbTodos =
    case mbTodos of 
        Just todos 
            -> List.map(\s -> viewToDo s) todos |> div []
        Nothing -> div [] [ text "No ToDos found"]

viewToDo : ToDo -> Html msg
viewToDo todo =
    div [] [ text todo.name ]
