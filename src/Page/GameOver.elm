module Page.GameOver exposing (view)

import Html exposing (Html, button, details, div, h1, h2, span, summary, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import State exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "red" ] [ text "Game Over" ]
        , button [ onClick NewGame ] [ text "New Game" ]
        ]
