module Deck exposing (Deck, deal, values)

import Array
import Random   
import Set
import Card

type alias Deck = List Card.Card

deal : Deck -> Deck -> Maybe (Deck, Deck, Card.Card)
deal deck discard =
    case (deck, discard) of
        ((card::rest), _) -> Just (rest, discard, card)
        (_, (card::rest)) -> Just (rest, deck, card) 
        _ -> Nothing

values : Deck -> Set.Set Int
values deck = 
    case deck of
        (card::rest) ->
            let
                otherValues = values rest
                thisValues = Set.fromList <| Card.values card
                allCardValues = Set.foldl 
                    (\thisValue a -> Set.foldl 
                        (\otherValue s -> Set.insert (otherValue + thisValue) s) 
                        Set.empty 
                        otherValues
                    ) Set.empty thisValues
            in
            allCardValues
        _ -> Set.empty