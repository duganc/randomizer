module Randomizer exposing (..)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (filter, join)
import Random



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { result : RandomResult
    , customText : String
    , submitButton : SubmitButtonState
    }


type SubmitButtonState
    = Allowed Float
    | Disallowed


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model None "" Disallowed, Cmd.none )



-- UPDATE


type Msg
    = RandomNumber Float
    | Submit RandomResult
    | CustomTextChange String
    | CustomButtonSubmit


type RandomResult
    = None
    | Success
    | Failure
    | Error


generateRandom : Float -> Cmd Msg
generateRandom f =
    Random.generate Submit (Random.weighted ( f, Success ) [ ( 1.0 - f, Failure ) ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomNumber f ->
            ( model
            , generateRandom f
            )

        Submit r ->
            ( { model | result = r }
            , Cmd.none
            )

        CustomTextChange s ->
            ( { model | submitButton = getSubmitButtonState s, customText = s }
            , Cmd.none
            )

        CustomButtonSubmit ->
            case model.submitButton of
                Allowed f ->
                    ( model, generateRandom f )

                Disallowed ->
                    ( { model | result = Error }, Cmd.none )


maybeParsePercentage : String -> Maybe Float
maybeParsePercentage =
    String.toInt >> Maybe.map (\n -> toFloat n / 100.0)


validatePercentage : Maybe Float -> Maybe Float
validatePercentage =
    Maybe.Extra.filter (\f -> f >= 0.0 && f <= 1.0)


getSubmitButtonState : String -> SubmitButtonState
getSubmitButtonState s =
    case s |> maybeParsePercentage |> validatePercentage of
        Just f ->
            Allowed f

        Nothing ->
            Disallowed



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


getResultDiv : Model -> Html Msg
getResultDiv model =
    case model.result of
        Success ->
            div [ style "color" "green" ] [ text "SUCCESS!" ]

        Failure ->
            div [ style "color" "red" ] [ text "FAILURE!" ]

        None ->
            div [ style "color" "black" ] [ text "Click a button" ]

        Error ->
            div [ style "color" "red" ] [ text "ERROR!" ]


toPercentageString : Float -> String
toPercentageString f =
    String.fromInt (round (f * 100)) ++ " / " ++ String.fromInt (round ((1 - f) * 100))


generateButton : Float -> Html Msg
generateButton f =
    if f < 0.0 || f > 1.0 then
        div [] [ text ("generateButton " ++ String.fromFloat f ++ " is invalid") ]

    else
        button [ onClick (RandomNumber f) ] [ text (toPercentageString f) ]


getSubmitMessage : SubmitButtonState -> String
getSubmitMessage s =
    case s of
        Allowed _ ->
            "Submit"

        Disallowed ->
            "Invalid Entry"


getSubmitButton : SubmitButtonState -> Html Msg
getSubmitButton s =
    button [ onClick CustomButtonSubmit ] [ text (getSubmitMessage s) ]


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
        , div [] []
        , input [ placeholder "Custom %", value model.customText, onInput CustomTextChange ] []
        , getSubmitButton model.submitButton
        ]
