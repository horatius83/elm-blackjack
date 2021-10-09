module Deck exposing (Deck, deal, new, values)

import Array
import Card
import Random
import Random.List
import Set


type alias Deck =
    List Card.Card


new : Int -> Deck
new numberOfDecks =
    let
        generateDeck =
            List.map
                (\r ->
                    List.map
                        (\s -> Card.Card r s)
                        Card.getAllSuits
                )
                Card.getAllRanks
                |> List.concat
    in
    if numberOfDecks < 1 then
        []

    else
        List.map (\_ -> generateDeck) (List.range 0 numberOfDecks) |> List.concat


deal : Deck -> Deck -> Maybe ( Deck, Deck, Card.Card )
deal deck discard =
    case ( deck, discard ) of
        ( card :: rest, _ ) ->
            Just ( rest, discard, card )

        ( _, card :: rest ) ->
            Just ( rest, deck, card )

        _ ->
            Nothing


values : Deck -> Set.Set Int
values deck =
    case deck of
        card :: rest ->
            let
                otherValues =
                    values rest

                thisValues =
                    Set.fromList <| Card.values card

                allCardValues =
                    Set.foldl
                        (\thisValue a ->
                            Set.foldl
                                (\otherValue s -> Set.insert (otherValue + thisValue) s)
                                Set.empty
                                otherValues
                        )
                        Set.empty
                        thisValues
            in
            allCardValues

        _ ->
            Set.empty


shuffle : Deck -> Random.Generator Deck
shuffle deck =
    Random.List.shuffle deck
