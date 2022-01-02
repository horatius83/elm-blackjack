module Page.Round exposing (..)

import Controls exposing (viewDealer, viewPlayer, viewRoundActions)
import Html exposing (Html, button, div, h1, text)
import State


view : State.Model -> Html State.Msg
view model =
    div []
        [ viewDealer model.dealer.cards False
        , viewPlayer model.player
        ]
