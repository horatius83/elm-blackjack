module Game exposing (..)
import Card
import Deck
import Hand
import Player

type SurrenderRules = No | Early | Late
type alias BlackJackPayout = {numerator: Int, denominator: Int}
type alias Rules = {
    minimumBet: Int, 
    maximumBet: Int, 
    blackJackPayout: BlackJackPayout, 
    numberOfSplits: Int, 
    numberOfDecks: Int, 
    surrenderRules: SurrenderRules}
type GameState = Init | PlaceBets | Round | RoundEnd | GameOver
type alias Game = {
    dealer: {cards: Deck.Deck},
    players: List Player.Player,
    deck: Deck.Deck,
    discard: Deck.Deck,
    state: GameState
    rules: Rules
    }

dealCard -> Deck.Deck -> Deck.Deck -> Deck.Deck -> (Deck.Deck, Deck.Deck, Deck.Deck)
dealCard deck to discard =
    case (deck, discard) of
        ((card:rest), _) -> (rest, card :: to, discard)
        (_, (card:rest)) -> (rest, card :: to, [])
