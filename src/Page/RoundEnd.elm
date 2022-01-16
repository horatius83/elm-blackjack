module Page.RoundEnd exposing (view)

import Array exposing (Array)
import Controls exposing (viewCards)
import Game exposing (getMaximumCardValue)
import Hand exposing (Hand)
import Html exposing (Html, div, h1, span, text)
import State exposing (Model, Msg)


viewDealer : Model -> Html Msg
viewDealer model =
    let
        cards =
            viewCards model.dealer.cards

        maxScore =
            getMaximumCardValue model.dealer.cards

        dealerStatus =
            case maxScore of
                Nothing ->
                    "Dealer Busted"

                Just score ->
                    "Dealer has " ++ String.fromInt score
    in
    div []
        [ h1 [] [ text dealerStatus ]
        , span [] cards
        ]


viewPlayerHand : Maybe Int -> ( Int, Hand ) -> Html Msg
viewPlayerHand dealerScore ( index, hand ) =
    let
        maximumPlayerValue =
            getMaximumCardValue hand.cards

        indexAsString =
            String.fromInt (index + 1)

        lostMessage =
            "Hand # " ++ indexAsString ++ " LOST"

        wonMessage =
            "Hand # " ++ indexAsString ++ " WON"

        handTitle =
            case ( dealerScore, maximumPlayerValue ) of
                ( Nothing, Nothing ) ->
                    lostMessage

                ( Just _, Nothing ) ->
                    lostMessage

                ( Nothing, Just _ ) ->
                    wonMessage

                ( Just dealer, Just player ) ->
                    if player > dealer then
                        wonMessage

                    else
                        lostMessage

        handTitleHtml =
            div [] [ text handTitle ]

        html =
            [ handTitleHtml ] ++ viewCards hand.cards
    in
    div [] html


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


view : Model -> Html Msg
view model =
    let
        dealerScore =
            getMaximumCardValue model.dealer.cards

        hands =
            model.player.hands
                |> viewPlayerHands dealerScore
    in
    div []
        [ viewDealer model
        , viewPlayerHands dealerScore model.player.hands
        , h1 [] [ text "Round End" ]
        ]