module Main exposing (main)

import Array
import Browser
import Card exposing (Card, Rank(..), Suit(..), cardBackHex, getCardFrontHex, getColor)
import Controls exposing (viewCard, viewInput, viewNumericInput, viewPayoutInput)
import Deck exposing (Deck)
import Game exposing (Game, GameState, default, defaultPlayer, new)
import Hand
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import List
import Page.Bet
import Page.Round
import Page.Rules
import Player exposing (addCardToHand)
import Random
import Random.List
import State exposing (Model, Msg(..))


init : () -> ( Model, Cmd Msg )
init _ =
    let
        startingMoney =
            1000

        model =
            Game.new defaultPlayer default
    in
    ( model, Random.generate NewDeck (Random.List.shuffle model.deck) )


view : Model -> Html Msg
view model =
    case model.state of
        Game.Init ->
            Page.Rules.view model

        Game.PlaceBets ->
            Page.Bet.view model

        Game.RoundStart ->
            Page.Round.view model

        _ ->
            div []
                [ button [ onClick ShuffleDeck ]
                    [ text "Shuffle List" ]
                , ul [] (List.map viewCard model.deck)
                , div [ attribute "class" "card" ] [ text cardBackHex ]
                ]


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

        ShuffleDiscard ->
            ( { model | deck = model.deck ++ model.discard, discard = [] }, Random.generate NewDeck (Random.List.shuffle model.deck) )

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

        ChangePlayerMoney money ->
            ( { model | player = { player_ | money = sToI defaultPlayer.money money } }, Cmd.none )

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

        ChangeBet bet ->
            let
                betAsInt =
                    sToI Game.default.minimumBet bet

                defaultHand =
                    Hand.create [] betAsInt

                oldHand =
                    Array.get 0 model.player.hands |> Maybe.withDefault defaultHand

                newHand =
                    { oldHand | bet = betAsInt }

                oldPlayer =
                    model.player

                newPlayer =
                    { oldPlayer | hands = Array.fromList [ newHand ] }
            in
            ( { model | player = newPlayer }, Cmd.none )

        ChangeGameState state ->
            case state of
                Game.PlaceBets ->
                    let
                        hand =
                            Hand.create [] model.rules.minimumBet

                        oldPlayer =
                            model.player

                        newPlayer =
                            { oldPlayer | hands = Array.fromList [ hand ] }
                    in
                    ( { model | state = Game.PlaceBets, player = newPlayer }, Cmd.none )

                Game.RoundStart ->
                    dealInitialCards model

                _ ->
                    ( { model | state = state }, Cmd.none )

        ShuffleDiscardIntoDeck nextMsg deck ->
            update nextMsg { model | deck = deck, discard = [] }


dealInitialCards : Model -> ( Model, Cmd Msg )
dealInitialCards model =
    let
        defaultHand =
            Hand.create [] model.rules.minimumBet

        hand =
            Array.get 0 model.player.hands
                |> Maybe.withDefault defaultHand

        oldDealer =
            model.dealer

        oldPlayer =
            model.player
    in
    case ( model.deck, model.dealer.cards, hand.cards ) of
        ( [], _, _ ) ->
            ( model, Random.generate (ShuffleDiscardIntoDeck (ChangeGameState Game.RoundStart)) <| Random.List.shuffle model.discard )

        ( x :: xs, [], [] ) ->
            let
                newDealer =
                    { oldDealer | cards = [ x ] }
            in
            dealInitialCards { model | dealer = newDealer, deck = xs }

        ( x :: xs, y :: [], [] ) ->
            let
                newHand =
                    { hand | cards = [ x ] }

                newPlayer =
                    { oldPlayer | hands = Array.fromList [ newHand ] }
            in
            dealInitialCards { model | player = newPlayer, deck = xs }

        ( x :: xs, y :: [], z :: [] ) ->
            let
                newDealer =
                    { oldDealer | cards = x :: [ y ] }
            in
            dealInitialCards { model | dealer = newDealer, deck = xs }

        ( x :: xs, _, z :: [] ) ->
            let
                newHand =
                    { hand | cards = z :: hand.cards }

                newPlayer =
                    { oldPlayer | hands = Array.fromList [ newHand ] }
            in
            ( { model | player = newPlayer, deck = xs, state = Game.RoundStart }, Cmd.none )

        ( _, _, _ ) ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
