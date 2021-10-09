module Main exposing (main)

import Browser
import Card exposing (Card, Rank(..), Suit(..), cardBackHex, getCardFrontHex, getColor)
import Deck
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import List
import Random
import Random.List
import String.UTF32 as UTF32


type alias Model =
    { cards : Deck.Deck
    }


type Msg
    = ShuffleDeck
    | NewDeck Deck.Deck


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards = Deck.new 1 }, Cmd.none )


displayCard : Card -> Html Msg
displayCard { rank, suit } =
    let
        color =
            getColor suit

        cardClass =
            "card " ++ color

        card =
            Card rank suit
    in
    span [ attribute "class" cardClass ] [ text (getCardFrontHex card) ]


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ShuffleDeck ]
            [ text "Shuffle List" ]
        , ul [] (List.map displayCard model.cards)
        , div [] [ text cardBackHex ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck ->
            ( model, Random.generate NewDeck (Random.List.shuffle model.cards) )

        NewDeck cards ->
            ( { model | cards = cards }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
