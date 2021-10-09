module Card exposing
    ( Card
    , Rank(..)
    , Suit(..)
    , cardBackHex
    , getAllRanks
    , getAllSuits
    , getCardFrontHex
    , getColor
    , values
    )

import Html exposing (Html)
import List
import String.UTF32 as UTF32


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type alias Card =
    { rank : Rank, suit : Suit }


values : Card -> List Int
values { rank, suit } =
    case rank of
        Two ->
            [ 2 ]

        Three ->
            [ 3 ]

        Four ->
            [ 4 ]

        Five ->
            [ 5 ]

        Six ->
            [ 6 ]

        Seven ->
            [ 7 ]

        Eight ->
            [ 8 ]

        Nine ->
            [ 9 ]

        Ten ->
            [ 10 ]

        Jack ->
            [ 10 ]

        Queen ->
            [ 10 ]

        King ->
            [ 10 ]

        Ace ->
            [ 1, 11 ]


getAllRanks : List Rank
getAllRanks =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]


getAllSuits : List Suit
getAllSuits =
    [ Clubs, Diamonds, Hearts, Spades ]


getCardFrontHex : Card -> String
getCardFrontHex { rank, suit } =
    let
        rankHex =
            case rank of
                Ace ->
                    0x01

                Two ->
                    0x02

                Three ->
                    0x03

                Four ->
                    0x04

                Five ->
                    0x05

                Six ->
                    0x06

                Seven ->
                    0x07

                Eight ->
                    0x08

                Nine ->
                    0x09

                Ten ->
                    0x0A

                Jack ->
                    0x0B

                Queen ->
                    0x0D

                King ->
                    0x0E

        suitHex =
            case suit of
                Spades ->
                    0xA0

                Hearts ->
                    0xB0

                Diamonds ->
                    0xC0

                Clubs ->
                    0xD0
    in
    UTF32.toString [ 0x0001F000 + suitHex + rankHex ]


cardBackHex : String
cardBackHex =
    UTF32.toString [ 0x0001F0A0 ]


getColor : Suit -> String
getColor suit =
    case suit of
        Spades ->
            "black"

        Hearts ->
            "red"

        Diamonds ->
            "red"

        Clubs ->
            "black"
