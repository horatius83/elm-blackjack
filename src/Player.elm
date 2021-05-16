module Player exposing (..)
import Hand exposing (Hand)

type alias Player = {
    name: String,
    hands: List Hand,
    money: Int
    }

new : String -> Int -> Player
new name money = Player name [] money