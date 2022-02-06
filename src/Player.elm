module Player exposing (..)

import Array
import Card exposing (Card)
import Hand exposing (Hand)


type alias Player =
    { name : String
    , hands : Array.Array Hand
    , money : Int
    , insured : Bool
    }


new : String -> Int -> Player
new name money =
    Player name Array.empty money False


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
