module Hand exposing (..)

import Deck exposing (Deck)


type alias Hand =
    { cards : Deck
    , bet : Int
    , stayed : Bool
    , doubleDown : Bool
    , surrendered : Bool
    }


create : Deck -> Int -> Hand
create deck bet =
    Hand deck bet False False False
