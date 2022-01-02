module Page.Round exposing (view)

import Array
import Card exposing (Card)
import Controls exposing (viewCard, viewCardBack, viewCards)
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, span, text)
import Html.Events exposing (onClick)
import Player exposing (Player)
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
        , text <| String.fromInt (List.length cards) ++ " cards"
        ]


viewHand : ( Int, Hand ) -> Html Msg
viewHand ( whichHand, hand ) =
    let
        handTitle =
            "Hand #" ++ String.fromInt (whichHand + 1)

        handTitleHtml =
            div [] [ text handTitle ]

        controls =
            div []
                [ span []
                    [ button [ onClick (Hit whichHand) ] [ text "Hit" ]
                    , button [] [ text "Stay" ]
                    , button [] [ text "Insurance" ]
                    , button [] [ text "Double Down" ]
                    , button [] [ text "Split" ]
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
