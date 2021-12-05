module Page.Rules exposing (..)

import Controls exposing (viewInput, viewNumericInput, viewPayoutInput)
import Game
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import State exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        stepValue =
            Just 10
    in
    div []
        [ h1 [] [ text "Game Rules" ]
        , div []
            [ div [] [ viewInput "Player Name: " "player_name" model.player.name "text" ChangePlayerName ]
            , div [] [ viewNumericInput "Minimum Bet: " "minimum_bet" model.rules.minimumBet stepValue (Just model.rules.maximumBet) stepValue ChangeMinimumBet ]
            , div [] [ viewNumericInput "Maximum bet: " "maximum_bet" model.rules.maximumBet (Just model.rules.minimumBet) Nothing stepValue ChangeMaximumBet ]
            , div [] [ viewNumericInput "Number of decks: " "number_of_decks" model.rules.numberOfDecks (Just 1) Nothing Nothing ChangeNumberOfDecks ]
            , div [] [ viewPayoutInput model.rules.blackJackPayout.numerator model.rules.blackJackPayout.denominator ]
            , div [] [ viewNumericInput "Number of Splits: " "number_of_splits" model.rules.numberOfSplits (Just 1) Nothing Nothing ChangeNumberOfSplits ]
            , div [] [ viewNumericInput "Starting money: " "money" model.player.money stepValue Nothing stepValue ChangePlayerMoney ]
            , div [] [ button [ onClick (ChangeGameState Game.PlaceBets) ] [ text "Start Game" ] ]
            ]
        ]
