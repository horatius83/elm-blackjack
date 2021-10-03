module Card exposing (
    Rank(..),
    Suit(..),
    Card,
    values, 
    getAllRanks, 
    getAllSuits, 
    getCardFrontHex, 
    cardBackHex, 
    getColor) 

import List
import String.UTF32 as UTF32

type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type Suit = Clubs | Diamonds | Hearts | Spades
type alias Card = {rank: Rank, suit: Suit}

values : Card -> List Int
values {rank, suit } =
    case rank of
        Two -> [2]
        Three -> [3]
        Four -> [4]
        Five -> [5]
        Six -> [6]
        Seven -> [7]
        Eight -> [8]
        Nine -> [9]
        Ten -> [10]
        Jack -> [10]
        Queen -> [10]
        King -> [10]
        Ace -> [1, 11]

getAllRanks : List Rank
getAllRanks = 
        [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

getAllSuits : List Suit
getAllSuits = [Clubs, Diamonds, Hearts, Spades]

getCardFrontHex : Card -> String
getCardFrontHex {rank, suit} =
    let
        rankHex =
            case rank of
                Ace -> 0x1
                Two -> 0x2
                Three -> 0x3
                Four -> 0x4
                Five -> 0x5
                Six -> 0x6
                Seven -> 0x7
                Eight -> 0x8
                Nine -> 0x9
                Ten -> 0xa
                Jack -> 0xb
                Queen -> 0xd
                King -> 0xe
        suitHex =
            case suit of
                Spades -> 0xa0
                Hearts -> 0xb0
                Diamonds -> 0xc0
                Clubs -> 0xd0
    in
        UTF32.toString [0x1f000 + suitHex + rankHex]

cardBackHex : String
cardBackHex = UTF32.toString [0x1f0a0]

getColor : Suit -> String
getColor suit =
    case suit of 
        Spades -> "black"
        Hearts -> "red"
        Diamonds -> "red"
        Clubs -> "black"