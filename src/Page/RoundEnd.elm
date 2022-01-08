module Page.RoundEnd exposing (view)

import Controls exposing (viewDealer, viewPlayer)
import Html exposing (Html, div, h1, text)
import State exposing (Model, Msg)


view : Model -> Html Msg
view model =
    div []
        [ viewDealer model.dealer.cards True
        , viewPlayer False model.player
        , h1 [] [ text "Round End" ]
        ]
