module Page.Rules exposing (..)

import Array
import Controls exposing (viewInput, viewNumericInput, viewPayoutInput, viewSelect)
import Game
import Html exposing (Html, button, div, h1, label, text)
import Html.Attributes exposing (attribute, class, disabled)
import Html.Events exposing (onClick)
import State exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        stepValue =
            Just 10

        optionToMsg option =
            ChangeSurrenderRules <|
                case option of
                    "Early" ->
                        Game.Early

                    "Late" ->
                        Game.Late

                    _ ->
                        Game.No

        surrenderOptions =
            [ "No", "Early", "Late" ]

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
                hand :: hands ->
                    hand.bet

                _ ->
                    model.rules.minimumBet
    in
    div [ class "play-area" ]
        [ h1 [] [ text "Game Rules" ]
        , div []
            [ div [] [ viewNumericInput "Minimum Bet: " "minimum_bet" model.rules.minimumBet stepValue (Just model.rules.maximumBet) stepValue ChangeMinimumBet ]
            , div [] [ viewNumericInput "Maximum bet: " "maximum_bet" model.rules.maximumBet (Just model.rules.minimumBet) Nothing stepValue ChangeMaximumBet ]
            , div [] [ viewNumericInput "Number of decks: " "number_of_decks" model.rules.numberOfDecks (Just 1) Nothing Nothing ChangeNumberOfDecks ]
            , div [] [ viewPayoutInput model.rules.blackJackPayout.numerator model.rules.blackJackPayout.denominator ]
            , div [] [ viewNumericInput "Number of Splits: " "number_of_splits" model.rules.numberOfSplits (Just 1) Nothing Nothing ChangeNumberOfSplits ]
            , div [] [ viewNumericInput "Starting money: " "money" model.player.money stepValue Nothing stepValue ChangePlayerMoney ]
            , viewSelect "surrender-rules" "Surrender" surrenderOptions optionToMsg
            , div [] [ viewNumericInput "Initial Bet: " "bet" bet minimumBet maximumBet stepValue ChangeBet ]
            , div []
                [ button
                    [ onClick (ChangeGameState Game.PlaceBets)
                    ]
                    [ text "Start Game" ]
                ]
            ]
        ]
