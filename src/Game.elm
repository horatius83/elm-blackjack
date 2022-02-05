module Game exposing (..)

import Card
import Deck exposing (Deck)
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

type HandResult 
    = Won
    | Lost
    | Pushed


default : Rules
default =
    Rules 100 1000 (BlackJackPayout 3 2) 1 2 No


defaultPlayer : Player
defaultPlayer =
    Player.new "Max" 1000


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
