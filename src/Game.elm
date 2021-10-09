module Game exposing (..)

import Deck
import Html exposing (s)
import Player


type SurrenderRules
    = No
    | Early
    | Late


type alias BlackJackPayout =
    { numerator : Int, denominator : Int }


type alias Rules =
    { minimumBet : Int
    , maximumBet : Int
    , blackJackPayout : BlackJackPayout
    , numberOfSplits : Int
    , numberOfDecks : Int
    , surrenderRules : SurrenderRules
    }


type GameState
    = Init
    | PlaceBets
    | Round
    | RoundEnd
    | GameOver


type alias Game =
    { dealer : { cards : Deck.Deck }
    , players : List Player.Player
    , deck : Deck.Deck
    , discard : Deck.Deck
    , state : GameState
    , rules : Rules
    }
