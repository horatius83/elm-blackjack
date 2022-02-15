module State exposing (..)

import Deck
import Game exposing (Game, GameState, SurrenderRules)


type alias Model =
    Game


type Msg
    = ShuffleDeck
    | ShuffleDiscard
    | ChangePlayerMoney String
    | ChangeMinimumBet String
    | ChangeMaximumBet String
    | ChangeNumberOfDecks String
    | ChangePayoutNumerator String
    | ChangePayoutDenominator String
    | ChangeNumberOfSplits String
    | ChangeBet String
    | ChangeSurrenderRules SurrenderRules
    | NewDeck Deck.Deck
    | ChangeGameState GameState
    | ShuffleDiscardIntoDeck Msg Deck.Deck
    | Hit Int
    | Stay Int
    | DoubleDown Int
    | Surrender
    | DealDealerCards
    | Split Int
    | Insure
    | NewRound
