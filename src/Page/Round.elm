module Page.Round exposing (view)

import Array
import Card exposing (Card)
import Controls exposing (viewCard, viewCardBack, viewCards)
import Game
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (attribute, disabled)
import Html.Events exposing (onClick)
import Player exposing (Player)
import Set
import State exposing (Msg(..))


viewDealer : List Card -> Bool -> Html Msg
viewDealer cards showAll =
    let
        cardsAsHtml =
            case ( cards, showAll ) of
                ( [], _ ) ->
                    []

                ( x :: xs, False ) ->
                    [ viewCardBack ] ++ viewCards xs

                ( x :: xs, True ) ->
                    [ viewCard x ] ++ viewCards xs
    in
    div []
        [ h1 [] [ text "Dealer" ]
        , span [] cardsAsHtml
        ]


viewHand : ( Int, Hand ) -> Html Msg
viewHand ( whichHand, hand ) =
    let
        handTitle =
            "Hand #" ++ String.fromInt (whichHand + 1)

        handTitleHtml =
            div [] [ text handTitle ]

        valuesAsText =
            Game.getCardValues hand.cards
                |> Set.map String.fromInt
                |> Set.toList
                |> String.join ", "

        controls =
            div []
                [ span []
                    [ button
                        [ onClick (Hit whichHand)
                        , disabled (Game.isBusted hand.cards)
                        ]
                        [ text "Hit" ]
                    , button
                        [ onClick (Stay whichHand)
                        ]
                        [ text "Stay" ]
                    , button [] [ text "Insurance" ]
                    , button [] [ text "Double Down" ]
                    , button [] [ text "Split" ]
                    , text valuesAsText
                    ]
                ]

        html =
            [ handTitleHtml ] ++ viewCards hand.cards ++ [ controls ]
    in
    div [] [ div [] html ]


viewHands : Array.Array Hand -> List (Html Msg)
viewHands hands =
    Array.toIndexedList hands
        |> List.map viewHand


viewPlayer : Player -> Html Msg
viewPlayer player =
    let
        hands =
            viewHands player.hands
    in
    div []
        [ h1 [] [ text player.name ]
        , div [] hands
        ]


view : State.Model -> Html State.Msg
view model =
    div []
        [ viewDealer model.dealer.cards False
        , viewPlayer model.player
        ]
