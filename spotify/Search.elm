module Search where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Events exposing (onChange, onEnter)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Signal exposing (message,forwardTo,Address)



-- MODEL


type alias Model =
    { queryText : String
    , queryType : String
    , answers : List Answer
    , loading : Bool
    }


type alias Answer =
    { name : String
    , queryType : String
    }


init : (Model, Effects Action)
init =
  ( Model "" "album" [] False
  , Effects.none
  )



-- UPDATE

type Action
    = QueryTextChange String
    | QueryTypeChange String
    | Submit
    | RegisterAnswers (Maybe (List Answer))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    QueryTextChange newQuery ->
      ( Model newQuery model.queryType model.answers model.loading
      , Effects.none
      )

    QueryTypeChange newType ->
      ( Model model.queryText newType model.answers model.loading
      , Effects.none
      )

    Submit ->
      ( { model | loading <- True}
      , search model.queryText model.queryType
      )

    RegisterAnswers maybeAnswers ->
      ( Model model.queryText model.queryType (Maybe.withDefault [] maybeAnswers) False
      , Effects.none
      )



-- VIEW


containerFluid =
  div [class "container-fluid"]


row =
  div [class "row"]


bootstrap =
  node "link"
    [ href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , rel "stylesheet"
    ]
    []


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [style [("margin", "20px 0")]]
    [ bootstrap
    , containerFluid
        [ inputForm address model
        , (if model.loading then (text "LOADING") else (text ""))
        , resultsList address model
        ]
    ]


inputForm address model =
  div []
  [input
    [ type' "text"
    , placeholder "Search for an album..."
    , value model.queryText
    , onChange address QueryTextChange
    ] []
   , select
   [ onChange address QueryTypeChange ]
   [ option [value "album"] [text "Album"]
   , option [value "artist"] [text "Artist"]]
   , button
  [ onClick address Submit ]
  [text "Submit"]]

resultsList address model =
  let
    toEntry answer =
      div
        [class "col-xs-2 col-md-3"]
        [resultView answer]
  in
    row (List.map toEntry model.answers)

resultView : Answer -> Html
resultView answer =
  div [class "panel panel-info"]
      [ div
          [class "panel-heading"]
          [text answer.queryType]
      , div
          [ class "panel-body"
          , style [("height", "10rem")]
          ]
          [text answer.name]
      ]



-- EFFECTS


(=>) = (,)


search : String -> String -> Effects Action
search query queryType =
  Http.get decodeAnswers (searchUrl query queryType)
    |> Task.toMaybe
    |> Task.map RegisterAnswers
    |> Effects.task


searchUrl : String -> String -> String
searchUrl query queryType =
  Http.url "https://api.spotify.com/v1/search"
    [ "q" => query
    , "type" => queryType
    ]


decodeAnswers : Json.Decoder (List Answer)
decodeAnswers =
  let
    albumName =
      Json.map (\n -> Answer n "Album") ("name" := Json.string)

    artistName =
      Json.map (\n -> Answer n "Artist") ("name" := Json.string)

  in
    Json.oneOf
          [
           (Json.at ["albums", "items"] (Json.list albumName))
           ,(Json.at ["artists", "items"] (Json.list artistName))
          ]
