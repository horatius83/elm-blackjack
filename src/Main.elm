module Main exposing (main)

import Array
import Browser
import Card exposing (Card, Rank(..), Suit(..), cardBackHex, getCardFrontHex, getColor)
import Controls exposing (viewCard, viewInput, viewNumericInput, viewPayoutInput)
import Deck exposing (Deck)
import Game exposing (Game, GameState, default, defaultPlayer, getMaximumCardValue, new)
import Hand exposing (Hand)
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import List
import Page.Bet
import Page.Round
import Page.RoundEnd
import Page.Rules
import Player exposing (addCardToHand)
import Random
import Random.List
import Set
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

        Game.RoundEnd ->
            Page.RoundEnd.view model

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
            changeBet model bet

        ChangeSurrenderRules rule ->
            ( { model | rules = { rules_ | surrenderRules = rule } }, Cmd.none )

        ChangeGameState state ->
            changeGameState model state

        ShuffleDiscardIntoDeck nextMsg deck ->
            update nextMsg { model | deck = deck, discard = [] }

        Hit hand ->
            hit model hand

        Stay hand ->
            stay model hand

        DoubleDown hand ->
            doubleDown model hand

        Split hand ->
            split model hand

        Surrender ->
            surrender model

        Insure ->
            insure model

        DealDealerCards ->
            dealDealerCards model

        NewRound ->
            newRound model


insure : Model -> ( Model, Cmd Msg )
insure model =
    let
        oldPlayer =
            model.player

        newPlayer =
            { oldPlayer | insured = True }

        newModel =
            { model | player = newPlayer }
    in
    ( newModel, Cmd.none )


changeBet : Model -> String -> ( Model, Cmd Msg )
changeBet model betAsString =
    let
        sToI d v =
            Maybe.withDefault d (String.toInt v)

        betAsInt =
            sToI Game.default.minimumBet betAsString

        oldHand =
            Array.get 0 model.player.hands |> Maybe.withDefault (defaultHand model)

        newHand =
            { oldHand | bet = betAsInt }

        oldPlayer =
            model.player

        newPlayer =
            { oldPlayer | hands = Array.fromList [ newHand ] }
    in
    ( { model | player = newPlayer }, Cmd.none )


changeGameState : Model -> Game.GameState -> ( Model, Cmd Msg )
changeGameState model state =
    case state of
        Game.PlaceBets ->
            let
                playerCards =
                    model.player.hands
                        |> Array.map (\x -> x.cards)
                        |> Array.toList
                        |> List.foldl (++) []

                newDiscard =
                    playerCards ++ model.dealer.cards ++ model.discard

                hand =
                    Hand.create [] model.rules.minimumBet

                oldPlayer =
                    model.player

                newPlayer =
                    { oldPlayer | hands = Array.fromList [ hand ] }

                oldDealer =
                    model.dealer

                newDealer =
                    { oldDealer | cards = [] }
            in
            ( { model | state = Game.PlaceBets, player = newPlayer, discard = newDiscard, dealer = newDealer }, Cmd.none )

        Game.RoundStart ->
            dealInitialCards model

        Game.RoundEnd ->
            roundEnd { model | state = state }

        _ ->
            ( { model | state = state }, Cmd.none )


roundEnd : Model -> ( Model, Cmd Msg )
roundEnd model =
    let
        maxDealerValue =
            getMaximumCardValue model.dealer.cards

        getBetResult =
            Game.getBetResult maxDealerValue

        playerWinnings =
            Array.map getBetResult model.player.hands
                |> Array.foldl (+) 0

        oldPlayer =
            model.player

        newPlayer =
            { oldPlayer | money = oldPlayer.money + playerWinnings }

        newModel =
            { model | player = newPlayer }
    in
    ( newModel, Cmd.none )


shuffleDiscard : Model -> Msg -> Cmd Msg
shuffleDiscard model nextMsg =
    Random.generate (ShuffleDiscardIntoDeck nextMsg) <| Random.List.shuffle model.discard


defaultHand : Model -> Hand.Hand
defaultHand model =
    Hand.create [] model.rules.minimumBet


changePlayerHand : Model -> Int -> (Hand -> Hand) -> Model
changePlayerHand model hand f =
    let
        oldHand =
            model.player.hands
                |> Array.get hand
                |> Maybe.withDefault (defaultHand model)

        newHand =
            f oldHand

        oldPlayer =
            model.player

        newHands =
            Array.set hand newHand model.player.hands

        newPlayer =
            { oldPlayer | hands = newHands }
    in
    { model | player = newPlayer }


hit : Model -> Int -> ( Model, Cmd Msg )
hit model hand =
    case model.deck of
        [] ->
            ( model, Hit hand |> shuffleDiscard model )

        card :: cards ->
            let
                hitHand =
                    \h -> { h | cards = card :: h.cards }

                newModel =
                    changePlayerHand model hand hitHand

                newNewModel =
                    { newModel | deck = cards }

                isBusted =
                    newNewModel.player.hands
                        |> Array.get hand
                        |> Maybe.withDefault (defaultHand model)
                        |> (\x -> x.cards)
                        |> Game.isBusted
            in
            if isBusted then
                stay newNewModel hand

            else
                ( newNewModel, Cmd.none )


stay : Model -> Int -> ( Model, Cmd Msg )
stay model hand =
    let
        stayHand =
            \h -> { h | stayed = True }

        newModel =
            changePlayerHand model hand stayHand

        allStayed =
            Array.map (\x -> x.stayed) newModel.player.hands
                |> Array.foldl (&&) True
    in
    if allStayed then
        dealDealerCards newModel

    else
        ( newModel, Cmd.none )


doubleDown : Model -> Int -> ( Model, Cmd Msg )
doubleDown model hand =
    let
        newModel =
            changePlayerHand model hand (\h -> { h | bet = h.bet * 2, doubleDown = True })
    in
    case model.deck of
        [] ->
            ( model, DoubleDown hand |> shuffleDiscard model )

        card :: cards ->
            hit model hand
                |> Tuple.first
                |> (\newNewModel -> stay newNewModel hand)


split : Model -> Int -> ( Model, Cmd Msg )
split model handIndex =
    let
        splitHand hand =
            case hand.cards of
                card :: cards ->
                    let
                        handA =
                            Hand.create [ card ] hand.bet

                        handB =
                            Hand.create cards hand.bet
                    in
                    Just ( handA, handB )

                [] ->
                    Nothing

        indexedList =
            Array.toIndexedList model.player.hands

        appendHands ( index, hand ) newHands =
            if index == handIndex then
                case splitHand hand of
                    Just ( handA, handB ) ->
                        List.append [ handA, handB ] newHands

                    Nothing ->
                        hand :: newHands

            else
                hand :: newHands

        newHands2 =
            List.foldl appendHands [] indexedList
                |> Array.fromList

        oldPlayer =
            model.player

        newPlayer =
            { oldPlayer | hands = newHands2 }

        newModel =
            { model | player = newPlayer }
    in
    ( newModel, Cmd.none )


surrender : Model -> ( Model, Cmd Msg )
surrender model =
    let
        newModel =
            changePlayerHand model 0 (\h -> { h | surrendered = True })
    in
    stay newModel 0


dealDealerCards : Model -> ( Model, Cmd Msg )
dealDealerCards model =
    let
        validCardCounts =
            Game.getCardValues model.dealer.cards
                |> Set.filter (\x -> x < 22)
                |> Set.toList

        maxCardCount =
            validCardCounts
                |> List.foldl
                    (\max x ->
                        if x > max then
                            x

                        else
                            max
                    )
                    0
    in
    case ( validCardCounts, maxCardCount < 17, model.deck ) of
        ( [], _, _ ) ->
            changeGameState model Game.RoundEnd

        ( _, True, [] ) ->
            ( model, shuffleDiscard model DealDealerCards )

        ( _, True, card :: cards ) ->
            let
                oldDealer =
                    model.dealer

                newCards =
                    card :: oldDealer.cards

                newDealer =
                    { oldDealer | cards = newCards }
            in
            dealDealerCards { model | dealer = newDealer, deck = cards }

        ( _, False, _ ) ->
            changeGameState model Game.RoundEnd


dealInitialCards : Model -> ( Model, Cmd Msg )
dealInitialCards model =
    let
        hand =
            Array.get 0 model.player.hands
                |> Maybe.withDefault (defaultHand model)

        oldDealer =
            model.dealer

        oldPlayer =
            model.player
    in
    case ( model.deck, model.dealer.cards, hand.cards ) of
        ( [], _, _ ) ->
            ( model, ChangeGameState Game.RoundStart |> shuffleDiscard model )

        ( card :: cards, [], [] ) ->
            let
                newDealer =
                    { oldDealer | cards = [ card ] }
            in
            dealInitialCards { model | dealer = newDealer, deck = cards }

        ( card :: cards, dealerCard :: [], [] ) ->
            let
                newHand =
                    { hand | cards = [ card ] }

                newPlayer =
                    { oldPlayer | hands = Array.fromList [ newHand ] }
            in
            dealInitialCards { model | player = newPlayer, deck = cards }

        ( card :: cards, dealerCard :: [], playerCard :: [] ) ->
            let
                newDealer =
                    { oldDealer | cards = card :: [ dealerCard ] }
            in
            dealInitialCards { model | dealer = newDealer, deck = cards }

        ( card :: cards, dealerCards, playerCard :: [] ) ->
            let
                newHand =
                    { hand | cards = card :: hand.cards }

                newPlayer =
                    { oldPlayer | hands = Array.fromList [ newHand ] }

                dealerHasBlackJack =
                    case getMaximumCardValue dealerCards of
                        Just 21 ->
                            True

                        _ ->
                            False
            in
            if dealerHasBlackJack then
                stay { model | player = newPlayer, deck = cards } 0

            else
                ( { model | player = newPlayer, deck = cards, state = Game.RoundStart }, Cmd.none )

        ( _, _, _ ) ->
            ( model, Cmd.none )


newRound : Model -> ( Model, Cmd Msg )
newRound model =
    let
        isPlayerOutOfMoney =
            model.player.money <= 0
    in
    if isPlayerOutOfMoney then
        changeGameState model Game.GameOver

    else
        changeGameState model Game.PlaceBets


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
