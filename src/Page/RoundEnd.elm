module Page.RoundEnd exposing (view)

import Array exposing (Array)
import Controls exposing (viewCards)
import Game exposing (Game, GameState, getMaximumCardValue)
import Hand exposing (Hand)
import Html exposing (Html, button, div, h1, h2, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import State exposing (Model, Msg)


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


viewPlayerMoney : Model -> Html Msg
viewPlayerMoney model =
    let
        moneyAsString =
            String.fromInt model.player.money

        moneyAsHtml =
            "$"
                ++ moneyAsString
                |> text
    in
    div []
        [ h2 [] [ text model.player.name ]
        , moneyAsHtml
        ]


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
        , viewPlayerMoney model
        , button [ onClick State.NewRound ] [ text "New Round" ]
        ]
