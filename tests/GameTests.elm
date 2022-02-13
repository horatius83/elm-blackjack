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
            , test "Win: Dealer busts" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Ace Spades, Card Queen Hearts] bet
                        dealerHand = [Card Eight Clubs, Card Eight Hearts, Card Eight Diamonds]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal bet
            , test "Lose: Player busts" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Eight Clubs, Card Eight Hearts, Card Eight Diamonds] bet
                        dealerHand = [Card Ace Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal (-bet)
            , test "Lose: Both Dealer and Player busts" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Eight Clubs, Card Eight Hearts, Card Eight Diamonds] bet
                        dealerHand = [Card Ace Spades, Card Queen Hearts, Card King Clubs, Card Three Spades]
                    in
                    Game.getBetResult dealerHand playerHand
                        |> Expect.equal (-bet)
            , test "Win, Double-Down: Dealer has lower hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Ten Clubs, Card Eight Hearts] bet
                        playerHandWithDoubleDown = { playerHand | doubleDown = True, bet = bet * 2}
                        dealerHand = [Card Seven Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerHandWithDoubleDown
                        |> Expect.equal (bet * 2)
            , test "Lose, Double-Down: Dealer has higher hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Ten Clubs, Card Eight Hearts] bet
                        playerHandWithDoubleDown = { playerHand | doubleDown = True, bet = bet * 2}
                        dealerHand = [Card Ten Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerHandWithDoubleDown
                        |> Expect.equal (-bet * 2)
            , test "Surrender: Dealer has higher hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        halfBet = (\x -> x * -1) <| Game.getHalfBet bet
                        playerHand = Hand.create [Card Ten Clubs, Card Eight Hearts] bet
                        playerWithSurrender = { playerHand | surrendered = True }
                        dealerHand = [Card Ten Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerWithSurrender
                        |> Expect.equal halfBet
            , test "Surrender: Dealer has lower hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        halfBet = (\x -> x * -1) <| Game.getHalfBet bet
                        playerHand = Hand.create [Card Ten Clubs, Card Ace Hearts] bet
                        playerWithSurrender = { playerHand | surrendered = True }
                        dealerHand = [Card Ten Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerWithSurrender
                        |> Expect.equal halfBet
            , test "Insurance: Dealer has a blackjack and player does not" <|
                \_ -> 
                    let
                        bet = 100
                        halfBet = Game.getHalfBet bet
                        expectedBet = bet - (2 * halfBet)
                        playerHand = Hand.create [Card Ten Clubs, Card Eight Hearts] bet
                        playerWithInsurance = { playerHand | insurance = True }
                        dealerHand = [Card Ten Spades, Card Ace Hearts]
                    in
                    Game.getBetResult dealerHand playerWithInsurance
                        |> Expect.equal expectedBet
            , test "Insurance: Dealer has a blackjack and so does the player" <|
                \_ -> 
                    let
                        bet = 100
                        playerHand = Hand.create [Card Ten Clubs, Card Ace Hearts] bet
                        playerWithInsurance = { playerHand | insurance = True }
                        dealerHand = [Card Ten Spades, Card Ace Hearts]
                    in
                    Game.getBetResult dealerHand playerWithInsurance
                        |> Expect.equal bet
            , test "Insurance: Dealer does not have a blackjack but has a better hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        halfBet = (\x -> x * -1) <| Game.getHalfBet bet
                        expectedBet = -bet + halfBet
                        playerHand = Hand.create [Card Ten Clubs, Card Eight Hearts] bet
                        playerWithInsurance = { playerHand | insurance = True }
                        dealerHand = [Card Ten Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerWithInsurance
                        |> Expect.equal expectedBet
            , test "Insurance: Dealer does not have a blackjack and has a worse hand than Player" <|
                \_ -> 
                    let
                        bet = 100
                        halfBet = (\x -> x * -1) <| Game.getHalfBet bet
                        expectedBet = bet + halfBet
                        playerHand = Hand.create [Card Ten Clubs, Card Ace Hearts] bet
                        playerWithInsurance = { playerHand | insurance = True }
                        dealerHand = [Card Ten Spades, Card Queen Hearts]
                    in
                    Game.getBetResult dealerHand playerWithInsurance
                        |> Expect.equal expectedBet
            ]
        ]