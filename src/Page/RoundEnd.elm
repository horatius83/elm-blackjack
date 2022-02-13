module Page.RoundEnd exposing (view)

import Array exposing (Array)
import Controls exposing (viewCards, viewNumericInput)
import Game exposing (Game, GameState, getBetResult, getMaximumCardValue)
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, h2, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import State exposing (Model, Msg(..))
import Tuple exposing (second)


viewDealer : Model -> Html Msg
viewDealer model =
    let
        cards =
            viewCards model.dealer.cards
    in
    div [ class "dealer-area" ] <| cards


viewPlayerHand : Maybe Int -> ( Int, Hand ) -> Html Msg
viewPlayerHand dealerScore ( index, hand ) =
    let
        maximumPlayerValue =
            getMaximumCardValue hand.cards

        indexAsString =
            String.fromInt (index + 1)
    in
    div [ class "player-area" ] <| viewCards hand.cards


viewPlayerHands : Maybe Int -> Array Hand -> Html Msg
viewPlayerHands dealerScore hands =
    let
        vh =
            viewPlayerHand dealerScore

        childElements =
            Array.toIndexedList hands
                |> List.map vh
    in
    div [] childElements


viewResults : Model -> Html Msg
viewResults model =
    let
        getResultClass result =
            if result > 0 then
                "hand-won"

            else if result == 0 then
                "hand-pushed"

            else
                "hand-lost"

        formatResult result =
            let
                resultAsString =
                    String.fromInt <| abs result
            in
            if result > 0 then
                "+$" ++ resultAsString

            else if result == 0 then
                " $" ++ resultAsString

            else
                "-$" ++ resultAsString

        handResults =
            model.player.hands
                |> Array.toList
                |> List.indexedMap (\i h -> ( i, getBetResult model.dealer.cards h ))

        totalResult =
            List.map second handResults
                |> List.foldl (+) 0

        results =
            case handResults of
                ( _, result ) :: [] ->
                    [ span [ class (getResultClass result) ] [ text (formatResult result) ] ]

                ( index, result ) :: otherResults ->
                    List.map (\( i, r ) -> span [ class (getResultClass r) ] [ text (formatResult r) ]) handResults

                [] ->
                    []

        playerMoney =
            "$" ++ String.fromInt model.player.money

        viewTotalResult =
            span [ class (getResultClass totalResult) ] [ text (formatResult totalResult) ]
    in
    div [ class "round-results" ]
        ([ text playerMoney ] ++ results)


view : Model -> Html Msg
view model =
    let
        dealerScore =
            getMaximumCardValue model.dealer.cards

        hands =
            model.player.hands
                |> viewPlayerHands dealerScore

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
                h :: _ ->
                    h.bet

                _ ->
                    model.rules.minimumBet
    in
    div []
        [ viewDealer model
        , viewPlayerHands dealerScore model.player.hands
        , viewResults model
        , div [] [ viewNumericInput "Bet: " "bet" bet minimumBet maximumBet stepValue ChangeBet ]
        , button [ onClick State.NewRound ] [ text "New Round" ]
        ]
