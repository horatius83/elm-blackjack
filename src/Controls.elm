module Controls exposing (..)

import Array
import Card exposing (Card, cardBackHex, getCardFrontHex, getColor)
import Game
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, input, label, option, select, span, text)
import Html.Attributes exposing (attribute, disabled)
import Html.Events exposing (onClick, onInput)
import List
import Player exposing (Player)
import Set
import State exposing (Model, Msg(..))


viewSelect : String -> String -> List ( Msg, String ) -> Html Msg
viewSelect id labelText options =
    let
        viewOption ( msg, txt ) =
            option [ attribute "value" txt ] [ text txt ]

        optionsAsHtml =
            List.map viewOption options
    in
    div []
        [ label [ attribute "for" id ] [ text labelText ]
        , select
            [ attribute "name" id
            , attribute "id" id
            ]
            optionsAsHtml
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
