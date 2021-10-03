module Main exposing (main)
import Card exposing (Rank(..), Suit(..), Card, getCardFrontHex, cardBackHex)
import String.UTF32 as UTF32

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.List
import List

type alias Model = List Int

cardBack = getCardFrontHex (Card Ace Spades)
test = UTF32.toString [ 0x1f0a1 ]

init : () -> (Model, Cmd Msg)
init _ = ([1, 2, 3, 4, 5, 6], Cmd.none)

displayNumber : Int -> Html Msg
displayNumber number = li [] [text (String.fromInt number)]

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ShuffleNumbers ]
            [text "Shuffle List" ]
        , ul [] (List.map displayNumber model)
        , div [] [text cardBack]
        , div [] [text cardBackHex]
        , div [] [text test]
        , div [] [text "Hello World"]
        ]

type Msg = ShuffleNumbers | NewList Model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShuffleNumbers -> (model, Random.generate NewList (Random.List.shuffle model))
        NewList numbers -> (numbers, Cmd.none)

main : Program () Model Msg
main =
    Browser.element
    {
        init = init,
        view = view,
        update = update,
        subscriptions = \_ -> Sub.none
    }



