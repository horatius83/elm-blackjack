module Player exposing (..)

import Array
import Card exposing (Card)
import Hand exposing (Hand)


type alias Player =
    { hands : Array.Array Hand
    , money : Int
    , insured : Bool
    , nextBet : Int
    }


new : Int -> Int -> Player
new money nextBet =
    Player Array.empty money False nextBet


addCardToHand : Player -> Int -> Card -> Maybe Player
addCardToHand p i c =
    let
        hand =
            Array.get i p.hands

        newCards =
            Maybe.map (\h -> c :: h.cards) hand

        newHand =
            Maybe.map2 (\h ncs -> { h | cards = ncs }) hand newCards

        newHands =
            Maybe.map (\nh -> Array.set i nh p.hands) newHand
    in
    Maybe.map (\nhs -> { p | hands = nhs }) newHands
