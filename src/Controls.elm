module Controls exposing (..)

import Array
import Card exposing (Card, cardBackHex, getCardFrontHex, getColor)
import Game
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, input, label, span, text)
import Html.Attributes exposing (attribute, disabled)
import Html.Events exposing (onClick, onInput)
import List
import Player exposing (Player)
import Set
import State exposing (Model, Msg(..))


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


viewHand : Bool -> ( Int, Hand ) -> Html Msg
viewHand showControls ( whichHand, hand ) =
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
            if showControls then
                [ handTitleHtml ] ++ viewCards hand.cards ++ [ controls ]

            else
                [ handTitleHtml ] ++ viewCards hand.cards
    in
    div [] [ div [] html ]


viewHands : Bool -> Array.Array Hand -> List (Html Msg)
viewHands showControls hands =
    let
        vh =
            viewHand showControls
    in
    Array.toIndexedList hands
        |> List.map vh


viewPlayer : Bool -> Player -> Html Msg
viewPlayer showControls player =
    let
        hands =
            viewHands showControls player.hands
    in
    div []
        [ h1 [] [ text player.name ]
        , div [] hands
        ]


viewInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewInput label_ id_ value_ type_ toMsg =
    span []
        [ label [ attribute "for" id_ ] [ text label_ ]
        , input
            [ attribute "value" value_
            , onInput toMsg
            , attribute "id" id_
            , attribute "name" id_
            , attribute "type" type_
            ]
            []
        ]


viewNumericInput : String -> String -> Int -> Maybe Int -> Maybe Int -> Maybe Int -> (String -> Msg) -> Html Msg
viewNumericInput label_ id_ value_ min_ max_ step_ toMsg =
    let
        defaultAttributes =
            [ attribute "value" <| String.fromInt value_
            , attribute "id" id_
            , attribute "name" id_
            , attribute "type" "number"
            , onInput toMsg
            ]

        toAttributeList attributeName maybeValue =
            case maybeValue of
                Just x ->
                    [ attribute attributeName (String.fromInt x) ]

                Nothing ->
                    []

        minAttribute =
            toAttributeList "min" min_

        maxAttribute =
            toAttributeList "max" max_

        stepAttribute =
            toAttributeList "step" step_

        attributes =
            defaultAttributes ++ minAttribute ++ maxAttribute ++ stepAttribute
    in
    span []
        [ label [ attribute "for" id_ ] [ text label_ ]
        , input attributes []
        ]


viewPayoutInput : Int -> Int -> Html Msg
viewPayoutInput numerator denominator =
    span []
        [ label [] [ text "BlackJack Payout: " ]
        , input
            [ attribute "value" (String.fromInt numerator)
            , attribute "type" "number"
            , onInput State.ChangePayoutNumerator
            ]
            []
        , text " to "
        , input
            [ attribute "value" (String.fromInt denominator)
            , attribute "type" "number"
            , onInput State.ChangePayoutDenominator
            ]
            []
        ]


viewCard : Card -> Html Msg
viewCard { rank, suit } =
    let
        color =
            getColor suit

        cardClass =
            "card " ++ color

        card =
            Card rank suit
    in
    span [ attribute "class" cardClass ] [ text (getCardFrontHex card) ]


viewCards : List Card -> List (Html Msg)
viewCards cards =
    List.map viewCard cards


viewCardBack : Html Msg
viewCardBack =
    span [ attribute "class" "card" ] [ text <| cardBackHex ]
