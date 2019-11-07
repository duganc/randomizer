module Randomizer exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Random

-- MAIN

main =
 Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

-- MODEL

type alias Model =
 { result : RandomResult }

init : () -> ( Model, Cmd Msg )
init _ = (Model None, Cmd.none )

-- UPDATE

type Msg = RandomNumber Float | Submit RandomResult
type RandomResult = None | Success | Failure

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
 case msg of
  RandomNumber f ->
   ( model
   , Random.generate Submit (Random.weighted (f, Success) [ (1.0 - f, Failure) ])
   )
  Submit r ->
   ( Model r
   , Cmd.none
   )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

getResultDiv : Model -> Html Msg
getResultDiv model =
 if model.result == Success then
  div [ style "color" "green" ] [ text "SUCCESS!" ]
 else
  div [ style "color" "red" ] [ text "FAILURE!" ]

toPercentageString : Float -> String
toPercentageString f =
 (String.fromInt (round (f * 100))) ++ " / " ++ (String.fromInt (round ((1 - f) * 100)))

generateButton : Float -> Html Msg
generateButton f =
 if f < 0.0 || f > 1.0 then
  div [] [ text ("generateButton " ++ (String.fromFloat f) ++ " is invalid") ]
 else
  button [ onClick (RandomNumber f) ] [ text (toPercentageString f) ]

view : Model -> Html Msg
view model = 
 div []
  [ getResultDiv model
  , generateButton 0.99
  , generateButton 0.9
  , generateButton 0.75
  , generateButton 0.5
  , generateButton 0.25
  , generateButton 0.1
  , generateButton 0.01
  ]