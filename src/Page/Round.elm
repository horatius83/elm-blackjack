module Page.Round exposing (view)

import Array exposing (Array)
import Card exposing (Card)
import Controls exposing (viewCardBack, viewCards)
import Game exposing (getCardValues)
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (attribute, disabled)
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
    div []
        [ h1 [] [ text "Dealer" ]
        , span [] cardsAsHtml
        ]


viewHand : Player -> ( Int, Hand ) -> Html Msg
viewHand player ( whichHand, hand ) =
    let
        handTitle =
            "Hand #" ++ String.fromInt (whichHand + 1)

        handTitleHtml =
            div [] [ text handTitle ]

        valuesAsText =
            getCardValues hand.cards
                |> Set.map String.fromInt
                |> Set.toList
                |> String.join ", "

        controls =
            div []
                [ span []
                    [ button
                        [ onClick (Hit whichHand)
                        , disabled (cannotHit player whichHand)
                        ]
                        [ text "Hit" ]
                    , button
                        [ onClick (Stay whichHand)
                        , disabled (cannotStand player whichHand)
                        ]
                        [ text "Stand" ]
                    , button [] [ text "Insurance" ]
                    , button [ onClick (DoubleDown whichHand) 
                             , disabled (cannotDoubleDown player whichHand)
                            ] [ text "Double Down" ]
                    , button [ onClick (Split whichHand)
                             , disabled (cannotSplit  player whichHand)
                        ] [ text "Split" ]
                    , text valuesAsText
                    ]
                ]

        html =
            [ handTitleHtml ] ++ viewCards hand.cards ++ [ controls ]
    in
    div [] [ div [] html ]


viewHands : Player -> List (Html Msg)
viewHands player =
    Array.toIndexedList player.hands
        |> List.map (viewHand player)


viewPlayer : Player -> Html Msg
viewPlayer player =
    let
        hands =
            viewHands player
    in
    div []
        [ h1 [] [ text player.name ]
        , div [] hands
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewDealer model.dealer.cards
        , viewPlayer model.player
        , text "Round"
        ]

cannotDoubleDown : Player -> Int -> Bool
cannotDoubleDown player handIndex =
    let
        playerHand = Array.get handIndex player.hands
        notEnoughMoney = Array.get handIndex player.hands
            |> Maybe.map (\h -> (h.bet * 2) > player.money)
        handIsBusted = Maybe.map (\h -> Game.isBusted h.cards) playerHand
    in
    case (Maybe.map2 (||) notEnoughMoney handIsBusted ) of
        Just x -> x
        Nothing -> True

cannotHit : Player -> Int -> Bool
cannotHit player handIndex =
    let
        hand = Array.get handIndex player.hands
        handIsBusted = Maybe.map (\h -> Game.isBusted h.cards) hand
        handIsStayed = Maybe.map (\h -> h.stayed) hand
    in
    case (handIsBusted, handIsStayed) of
        (Nothing, _) -> True
        (_, Nothing) -> True
        (Just x, Just y) -> x || y


cannotStand : Player -> Int -> Bool
cannotStand player handIndex = 
    let
        isStanding = Array.get handIndex player.hands
            |> Maybe.map (\h -> h.stayed)
    in
    case isStanding of
        Just x -> x
        Nothing -> True
        
cannotSplit : Player -> Int -> Bool
cannotSplit player handIndex =
    let
        hand = Array.get handIndex player.hands
        cardsAreSameValue cards = case cards of
            a :: b :: [] -> a.rank == b.rank
            _ -> False
        hasCardsOfSameValue = Maybe.map (\h -> cardsAreSameValue h.cards) hand
    in
    case hasCardsOfSameValue of
        Just x -> not x
        Nothing -> True