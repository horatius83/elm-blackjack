module Game exposing (..)

import Card
import Deck exposing (Deck)
import Hand exposing (Hand)
import Player exposing (Player)
import Set exposing (Set)


type SurrenderRules
    = No
    | Early
    | Late


type alias BlackJackPayout =
    { numerator : Int, denominator : Int }


type alias Rules =
    { minimumBet : Int
    , maximumBet : Int
    , blackJackPayout : BlackJackPayout
    , numberOfSplits : Int
    , numberOfDecks : Int
    , surrenderRules : SurrenderRules
    }


type GameState
    = Init
    | PlaceBets
    | RoundStart
    | PlayerStart
    | RoundEnd
    | GameOver


type alias Game =
    { dealer : { cards : Deck }
    , player : Player
    , deck : Deck
    , discard : Deck
    , state : GameState
    , rules : Rules
    }


default : Rules
default =
    Rules 100 1000 (BlackJackPayout 3 2) 1 2 No


defaultPlayer : Int -> Player
defaultPlayer bet =
    Player.new 1000 bet


new : Player -> Rules -> Game
new player rules =
    Game { cards = [] } player (Deck.new rules.numberOfDecks) [] Init rules


getCardValues : Deck -> Set Int
getCardValues deck =
    let
        getCardValuesAsList d =
            case d of
                [] ->
                    [ 0 ]

                card :: cards ->
                    let
                        values =
                            Card.values card

                        otherValues =
                            getCardValuesAsList cards
                    in
                    List.map (\x -> List.map (\y -> x + y) otherValues) values
                        |> List.foldl (++) []
    in
    getCardValuesAsList deck
        |> Set.fromList


getMaximumCardValue : Deck -> Maybe Int
getMaximumCardValue deck =
    let
        cardValuesUnder22 =
            getCardValues deck
                |> Set.filter (\x -> x < 22)
                |> Set.toList
    in
    case cardValuesUnder22 of
        [] ->
            Nothing

        xs ->
            Just <|
                List.foldl
                    (\x max ->
                        if x > max then
                            x

                        else
                            max
                    )
                    0
                    xs


isBusted : Deck -> Bool
isBusted deck =
    let
        values =
            getCardValues deck

        valuesUnder22 =
            Set.filter (\x -> x < 22) values
    in
    Set.isEmpty valuesUnder22


getHalfBet : Int -> Int
getHalfBet bet =
    bet
        |> toFloat
        |> (\x -> x / 2)
        |> round


getInsuranceBetResult : Deck -> Int -> Bool -> Int
getInsuranceBetResult dealerHand bet hasInsurance =
    let
        halfBet =
            getHalfBet bet

        doesDealerHaveBlackJack =
            let
                hasTwoCards =
                    List.length dealerHand == 2

                hasAnAce =
                    List.foldl (\c x -> x || c.rank == Card.Ace) False dealerHand

                hasATenCard =
                    List.map Card.values dealerHand
                        |> List.map (\vs -> List.foldl (\cardValue x -> x || cardValue == 10) False vs)
                        |> List.foldl (||) False
            in
            hasTwoCards && hasAnAce && hasATenCard
    in
    case ( hasInsurance, doesDealerHaveBlackJack ) of
        ( False, _ ) ->
            0

        ( True, True ) ->
            bet

        ( True, False ) ->
            -halfBet


getBetResult : Deck -> Hand -> Int
getBetResult dealerHand hand =
    let
        dealerHandValue =
            getMaximumCardValue dealerHand

        playerHandValue =
            getMaximumCardValue hand.cards

        halfBet =
            getHalfBet hand.bet
    in
    case ( playerHandValue, dealerHandValue, hand.surrendered ) of
        ( Nothing, Nothing, _ ) ->
            -hand.bet

        ( Nothing, Just dhv, _ ) ->
            -hand.bet

        ( Just phv, Nothing, True ) ->
            -halfBet

        ( Just phv, Nothing, False ) ->
            hand.bet

        ( Just phv, Just dhv, True ) ->
            -halfBet

        ( Just phv, Just dhv, False ) ->
            if phv > dhv then
                hand.bet

            else if phv == dhv then
                0

            else
                -hand.bet
