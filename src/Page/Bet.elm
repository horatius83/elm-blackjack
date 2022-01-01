module Page.Bet exposing (..)

import Array
import Controls exposing (viewNumericInput)
import Game
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import State exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        stepValue =
            Just 10

        minimumBet =
            Just model.rules.minimumBet

        maximumBet =
            Just <|
                if model.rules.maximumBet < model.player.money then
                    model.rules.maximumBet

                else
                    model.player.money

        bet =
            case Array.toList model.player.hands of
                x :: xs ->
                    x.bet

                _ ->
                    model.rules.minimumBet
    in
    div []
        [ h1 [] [ text "Place Bets" ]
        , div []
            [ viewNumericInput "Bet: " "bet" bet minimumBet maximumBet stepValue ChangeBet
            , div [] [ button [ onClick (ChangeGameState Game.RoundStart) ] [ text "PlaceBet" ] ]
            ]
        ]
