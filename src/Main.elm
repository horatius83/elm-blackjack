module Main exposing (main)

import Browser
import Card exposing (Card, Rank(..), Suit(..), cardBackHex, getCardFrontHex, getColor)
import Deck
import Game exposing (Game, GameState, default, new)
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import List
import Random
import Random.List


type alias Model =
    Game


type Msg
    = ShuffleDeck
    | ChangePlayerName String
    | ChangeMinimumBet String
    | ChangeMaximumBet String
    | ChangeNumberOfDecks String
    | ChangePayoutNumerator String
    | ChangePayoutDenominator String
    | ChangeNumberOfSplits String
    | SubmitRules
    | NewDeck Deck.Deck
    | StartGame


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            new "Max" 1000 default
    in
    ( model, Random.generate NewDeck (Random.List.shuffle model.deck) )


displayCard : Card -> Html Msg
displayCard { rank, suit } =
    let
        color =
            getColor suit

        cardClass =
            "card " ++ color

        card =
            Card rank suit
    in
    span [ attribute "class" cardClass ] [ text (getCardFrontHex card) ]


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


numericInput : String -> String -> Int -> Maybe Int -> Maybe Int -> Maybe Int -> (String -> Msg) -> Html Msg
numericInput label_ id_ value_ min_ max_ step_ toMsg =
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


payoutInput : Int -> Int -> Html Msg
payoutInput numerator denominator =
    span []
        [ label [] [ text "BlackJack Payout: " ]
        , input
            [ attribute "value" (String.fromInt numerator)
            , attribute "type" "number"
            , onInput ChangePayoutNumerator
            ]
            []
        , text " to "
        , input
            [ attribute "value" (String.fromInt denominator)
            , attribute "type" "number"
            , onInput ChangePayoutDenominator
            ]
            []
        ]


rulesView : Model -> Html Msg
rulesView model =
    div []
        [ h1 [] [ text "Game Rules" ]
        , form []
            [ div [] [ viewInput "Player Name: " "player_name" model.player.name "text" ChangePlayerName ]
            , div [] [ numericInput "Minimum Bet: " "minimum_bet" model.rules.minimumBet (Just 10) (Just model.rules.maximumBet) (Just 10) ChangeMinimumBet ]
            , div [] [ numericInput "Maximum bet: " "maximum_bet" model.rules.maximumBet (Just model.rules.minimumBet) Nothing (Just 10) ChangeMaximumBet ]
            , div [] [ numericInput "Number of decks: " "number_of_decks" model.rules.numberOfDecks (Just 1) Nothing Nothing ChangeNumberOfDecks ]
            , div [] [ payoutInput model.rules.blackJackPayout.numerator model.rules.blackJackPayout.denominator ]
            , div [] [ numericInput "Number of Splits: " "number_of_splits" model.rules.numberOfSplits (Just 1) Nothing Nothing ChangeNumberOfSplits ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model.state of
        Game.Init ->
            rulesView model

        _ ->
            div []
                [ button [ onClick ShuffleDeck ]
                    [ text "Shuffle List" ]
                , ul [] (List.map displayCard model.deck)
                , div [ attribute "class" "card" ] [ text cardBackHex ]
                ]



-- Round ->
-- RoundEnd ->
-- GameOver ->


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        sToI d v =
            Maybe.withDefault d (String.toInt v)

        rules_ =
            model.rules

        player_ =
            model.player

        blackJackPayout_ =
            rules_.blackJackPayout
    in
    case msg of
        ShuffleDeck ->
            ( model, Random.generate NewDeck (Random.List.shuffle model.deck) )

        NewDeck cards ->
            ( { model | deck = cards }, Cmd.none )

        ChangePlayerName name ->
            ( { model
                | player =
                    { player_
                        | name = name
                    }
              }
            , Cmd.none
            )

        ChangeMinimumBet bet ->
            ( { model | rules = { rules_ | minimumBet = sToI Game.default.minimumBet bet } }, Cmd.none )

        ChangeMaximumBet bet ->
            ( { model | rules = { rules_ | maximumBet = sToI Game.default.maximumBet bet } }, Cmd.none )

        ChangeNumberOfDecks decks ->
            ( { model | rules = { rules_ | numberOfDecks = sToI Game.default.numberOfDecks decks } }, Cmd.none )

        ChangePayoutNumerator numerator ->
            let
                numeratorAsInt =
                    sToI Game.default.blackJackPayout.numerator numerator
            in
            ( { model | rules = { rules_ | blackJackPayout = { blackJackPayout_ | numerator = numeratorAsInt } } }, Cmd.none )

        ChangePayoutDenominator denominator ->
            let
                denominatorAsInt =
                    sToI Game.default.blackJackPayout.denominator denominator
            in
            ( { model | rules = { rules_ | blackJackPayout = { blackJackPayout_ | denominator = denominatorAsInt } } }, Cmd.none )

        ChangeNumberOfSplits splits ->
            ( { model | rules = { rules_ | numberOfSplits = sToI Game.default.numberOfSplits splits } }, Cmd.none )

        SubmitRules ->
            ( model, Cmd.none )

        StartGame ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
