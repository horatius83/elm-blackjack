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


viewHand : ( Int, Hand ) -> Html Msg
viewHand ( whichHand, hand ) =
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
                        , disabled (Game.isBusted hand.cards)
                        ]
                        [ text "Hit" ]
                    , button
                        [ onClick (Stay whichHand)
                        ]
                        [ text "Stay" ]
                    , button [] [ text "Insurance" ]
                    , button [ onClick (DoubleDown whichHand) ] [ text "Double Down" ]
                    , button [] [ text "Split" ]
                    , text valuesAsText
                    ]
                ]

        html =
            [ handTitleHtml ] ++ viewCards hand.cards ++ [ controls ]
    in
    div [] [ div [] html ]


viewHands : Array Hand -> List (Html Msg)
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


view : Model -> Html Msg
view model =
    div []
        [ viewDealer model.dealer.cards
        , viewPlayer model.player
        , text "Round"
        ]
