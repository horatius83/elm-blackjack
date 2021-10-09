module Hand exposing (..)

import Deck exposing (Deck)


type alias Hand =
    { cards : Deck
    , bet : Int
    , insurance : Bool
    , stayed : Bool
    , doubleDown : Bool
    }


create : Deck -> Int -> Hand
create deck bet =
    Hand deck bet False False False
