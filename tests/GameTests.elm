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
            [ test "Loss: Dealer has a higher hand than Player" <| 
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Eight Clubs, Card Eight Hearts] bet
                        dealerHand = [Card Ace Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal (-bet)
            , test "Push: Dealer and Player have the same value hand" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Eight Clubs, Card Eight Hearts] bet
                        dealerHand = [Card Eight Clubs, Card Eight Hearts]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal 0
            , test "Win: Dealer has a lower hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Ace Spades, Card Queen Hearts] bet
                        dealerHand = [Card Eight Clubs, Card Eight Hearts]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal bet
            ]
        ]