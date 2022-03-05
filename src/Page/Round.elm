module Page.Round exposing (view)

import Array exposing (Array)
import Card exposing (Card)
import Controls exposing (viewCardBack, viewCards)
import Game exposing (Rules, getCardValues)
import Hand exposing (Hand)
import Html exposing (Html, button, details, div, h1, h2, span, summary, text)
import Html.Attributes exposing (attribute, class, disabled, hidden)
import Html.Events exposing (onClick, onInput)
import Player exposing (Player)
import Set
import State exposing (Model, Msg(..))


viewDealer : List Card -> Html Msg
viewDealer cards =
    let
        cardsAsHtml =
            case cards of
                [] ->
                    []

                _ :: xs ->
                    [ viewCardBack ] ++ viewCards xs
    in
    div [ class "dealer-area" ]
        [ span [] cardsAsHtml
        ]


viewHand : Model -> ( Int, Hand ) -> Html Msg
viewHand model ( whichHand, hand ) =
    let
        valuesAsText =
            getCardValues hand.cards
                |> Set.map String.fromInt
                |> Set.toList
                |> String.join ", "

        controls =
            div [ class "player-controls" ]
                [ span []
                    [ button
                        [ onClick (Hit whichHand)
                        , disabled (cannotHit model.player whichHand)
                        ]
                        [ text "Hit" ]
                    , button
                        [ onClick (Stay whichHand)
                        , disabled (cannotStand model.player whichHand)
                        ]
                        [ text "Stand" ]
                    , button
                        [ onClick (DoubleDown whichHand)
                        , disabled (cannotDoubleDown model.player whichHand)
                        ]
                        [ text "Double Down" ]
                    , button
                        [ onClick (Split whichHand)
                        , disabled (cannotSplit model.rules model.player whichHand)
                        ]
                        [ text "Split" ]
                    , button
                        [ onClick Surrender
                        , hidden (hideSurrender model)
                        , disabled (cannotSurrender model)
                        ]
                        [ text "Surrender" ]
                    , button
                        [ onClick Insure
                        , disabled (cannotInsure model)
                        ]
                        [ text "Insurance" ]
                    , text valuesAsText
                    ]
                ]

        cards =
            div [] <| viewCards hand.cards
    in
    div [ class "player-area" ]
        [ cards
        , controls
        ]


viewHands : Model -> List (Html Msg)
viewHands model =
    Array.toIndexedList model.player.hands
        |> List.map (viewHand model)


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        hands =
            viewHands model
    in
    div []
        [ div [] hands
        ]


viewRules : Model -> Html Msg
viewRules model =
    let
        blackjackPayoutAsString =
            String.fromInt model.rules.blackJackPayout.numerator ++ " : " ++ String.fromInt model.rules.blackJackPayout.denominator

        surrenderRulesAsString =
            case model.rules.surrenderRules of
                Game.No ->
                    "No"

                Game.Early ->
                    "Early"

                Game.Late ->
                    "Late"
    in
    details []
        [ summary [] [ text "Rules" ]
        , div [] [ text ("Minimum Bet: " ++ String.fromInt model.rules.minimumBet) ]
        , div [] [ text ("Maximum Bet: " ++ String.fromInt model.rules.maximumBet) ]
        , div [] [ text ("Blackjack Payout: " ++ blackjackPayoutAsString) ]
        , div [] [ text ("Max Number of Splits: " ++ String.fromInt model.rules.numberOfSplits) ]
        , div [] [ text ("Numer of Decks: " ++ String.fromInt model.rules.numberOfDecks) ]
        , div [] [ text ("Surrender Rules: " ++ surrenderRulesAsString) ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewDealer model.dealer.cards
        , viewPlayer model
        , viewRules model
        ]


cannotSurrender : Model -> Bool
cannotSurrender model =
    let
        handCount =
            Array.length model.player.hands

        hasInitialCards =
            Array.toList model.player.hands
                |> List.head
                |> Maybe.map (\x -> List.length x.cards)
                |> Maybe.map (\x -> x == 2)

        hasInitialHand =
            handCount == 1

        isSurrenderAllowed =
            model.rules.surrenderRules /= Game.No
    in
    case ( hasInitialHand, hasInitialCards, isSurrenderAllowed ) of
        ( True, Just True, True ) ->
            False

        _ ->
            True


hideSurrender : Model -> Bool
hideSurrender model =
    model.rules.surrenderRules == Game.No


cannotDoubleDown : Player -> Int -> Bool
cannotDoubleDown player handIndex =
    let
        playerHand =
            Array.get handIndex player.hands

        notEnoughMoney =
            Array.get handIndex player.hands
                |> Maybe.map (\h -> (h.bet * 2) > player.money)

        handIsBusted =
            Maybe.map (\h -> Game.isBusted h.cards) playerHand

        alreadyDoubledDown =
            Maybe.map (\h -> h.doubleDown) playerHand

        stayed =
            Maybe.map (\h -> h.stayed) playerHand
    in
    case Maybe.map4 (\m b d s -> m || b || d || s) notEnoughMoney handIsBusted alreadyDoubledDown stayed of
        Just x ->
            x

        Nothing ->
            True


cannotHit : Player -> Int -> Bool
cannotHit player handIndex =
    let
        hand =
            Array.get handIndex player.hands

        handIsBusted =
            Maybe.map (\h -> Game.isBusted h.cards) hand

        handIsStayed =
            Maybe.map (\h -> h.stayed) hand
    in
    case ( handIsBusted, handIsStayed ) of
        ( Just x, Just y ) ->
            x || y

        _ ->
            True


cannotStand : Player -> Int -> Bool
cannotStand player handIndex =
    let
        isStanding =
            Array.get handIndex player.hands
                |> Maybe.map (\h -> h.stayed)
    in
    case isStanding of
        Just x ->
            x

        Nothing ->
            True


cannotSplit : Rules -> Player -> Int -> Bool
cannotSplit rules player handIndex =
    let
        hand =
            Array.get handIndex player.hands

        numberOfHands =
            Array.length player.hands

        cardsAreSameValue cards =
            case cards of
                a :: b :: [] ->
                    a.rank == b.rank

                _ ->
                    False

        hasCardsOfSameValue =
            Maybe.map (\h -> cardsAreSameValue h.cards) hand

        doesNumberOfhandsExceedsNumberOfSplits =
            numberOfHands > rules.numberOfSplits
    in
    case ( hasCardsOfSameValue, doesNumberOfhandsExceedsNumberOfSplits ) of
        ( Just x, False ) ->
            not x

        _ ->
            True


cannotInsure : Model -> Bool
cannotInsure model =
    let
        getUpCard cards =
            case cards of
                [] ->
                    Nothing

                x :: [] ->
                    Nothing

                x :: y :: _ ->
                    Just y

        hand =
            Array.get 0 model.player.hands

        getHalfBet h =
            h.bet
                |> toFloat
                |> (\bet -> bet / 2)
                |> round

        halfBet =
            Maybe.map getHalfBet hand

        currentBet =
            Maybe.map (\h -> h.bet) hand

        totalBet =
            Maybe.map2 (\c h -> c + h) currentBet halfBet

        canBet =
            Maybe.map (\t -> t <= model.player.money) totalBet

        upCard =
            getUpCard model.dealer.cards
    in
    case ( canBet, upCard, model.player.insured ) of
        ( Just True, Just card, False ) ->
            card.rank /= Card.Ace

        _ ->
            True
