module Page.RoundEnd exposing (view)

import Controls exposing (viewDealer, viewPlayer)
import Html exposing (Html, div)
import State exposing (Model, Msg)


view : State.Model -> Html State.Msg
view model =
    div []
        [ viewDealer model.dealer.cards True
        , viewPlayer model.player
        ]
