module Deck exposing (..)

import Array
import Random   
import Card

type alias Deck = List Card.Card

deal : Deck -> Maybe (Deck, Card.Card)
deal deck =
    case deck of
        (card::rest) -> Just (rest, card)
        _ -> Nothing



