module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.List
import List

type alias Model = List Int

init : () -> (Model, Cmd Msg)
init _ = ([1, 2, 3, 4, 5, 6], Cmd.none)

displayNumber : Int -> Html Msg
displayNumber number = li [] [text (String.fromInt number)]

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (ShuffleNumbers model) ]
            [text "Shuffle List" ]
        , ul [] (List.map displayNumber model)
        ]

type Msg = ShuffleNumbers (List Int) | NewList (List Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShuffleNumbers numbers -> (model, Random.generate NewList (Random.List.shuffle model))
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



