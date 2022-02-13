module GameTests exposing (gameTests)

import Test exposing (..)
import Expect exposing (Expectation)
import Card exposing (Card, Rank(..), Suit(..))
import Game
import Hand

gameTests : Test
gameTests = 
    describe "The Game module"
        [ describe "getBetResult"
            [ test "Dealer has a higher hand than Player" <| 
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Eight Clubs, Card Eight Hearts] bet
                        dealerHand = [Card Ace Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal (-bet)
            ]
        ]