module Game exposing (..)

import Deck
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
    , player : Player.Player
    , deck : Deck.Deck
    , discard : Deck.Deck
    , state : GameState
    , rules : Rules
    }


default : Rules
default =
    Rules 100 1000 (BlackJackPayout 3 2) 1 2 No


defaultPlayer : Player.Player
defaultPlayer =
    Player.new "Max" 1000


new : Player.Player -> Rules -> Game
new player rules =
    Game { cards = [] } player (Deck.new rules.numberOfDecks) [] Init rules
