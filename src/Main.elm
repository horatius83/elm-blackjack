module Main exposing (main)

import Browser
import Card exposing (Card, Rank(..), Suit(..), cardBackHex, getCardFrontHex, getColor)
import Deck
import Game exposing (Game, GameState, default, new)
import Html exposing (..)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import List
import Random
import Random.List


type alias Model =
    Game


type Msg
    = ShuffleDeck
    | ChangePlayerName String
    | ChangeMinimumBet String
    | ChangeMaximumBet String
    | SubmitRules
    | NewDeck Deck.Deck
    | StartGame


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            new "Max" 1000 default
    in
    ( model, Random.generate NewDeck (Random.List.shuffle model.deck) )


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


viewInput : String -> String -> String -> String -> (String -> Msg) -> Html Msg
viewInput label_ id_ value_ type_ toMsg =
    span []
        [ label [ attribute "for" id_ ] [ text label_ ]
        , input
            [ attribute "value" value_
            , onInput toMsg
            , attribute "id" id_
            , attribute "name" id_
            , attribute "type" type_
            ]
            []
        ]


rulesView : Model -> Html Msg
rulesView model =
    div []
        [ h1 [] [ text "Game Rules" ]
        , form []
            [ div []
                [ viewInput "Player Name: " "player_name" model.player.name "text" ChangePlayerName ]
            , div []
                [ viewInput "Minimum Bet: " "minimum_bet" (String.fromInt model.rules.minimumBet) "number" ChangeMinimumBet ]
            , div []
                [ viewInput "Maximum Bet: " "maximum_bet" (String.fromInt model.rules.maximumBet) "number" ChangeMaximumBet ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model.state of
        Game.Init ->
            rulesView model

        _ ->
            div []
                [ button [ onClick ShuffleDeck ]
                    [ text "Shuffle List" ]
                , ul [] (List.map displayCard model.deck)
                , div [ attribute "class" "card" ] [ text cardBackHex ]
                ]



-- Round ->
-- RoundEnd ->
-- GameOver ->


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck ->
            ( model, Random.generate NewDeck (Random.List.shuffle model.deck) )

        NewDeck cards ->
            ( { model | deck = cards }, Cmd.none )

        ChangePlayerName name ->
            let
                player_ =
                    model.player
            in
            ( { model
                | player =
                    { player_
                        | name = name
                    }
              }
            , Cmd.none
            )

        ChangeMinimumBet bet ->
            let
                rules_ =
                    model.rules

                d =
                    Game.default

                betAsInt =
                    Maybe.withDefault d.minimumBet (String.toInt bet)
            in
            ( { model | rules = { rules_ | minimumBet = betAsInt } }, Cmd.none )

        ChangeMaximumBet bet ->
            let
                rules_ =
                    model.rules

                d =
                    Game.default

                betAsInt =
                    Maybe.withDefault d.maximumBet (String.toInt bet)
            in
            ( { model | rules = { rules_ | maximumBet = betAsInt } }, Cmd.none )

        SubmitRules ->
            ( model, Cmd.none )

        StartGame ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
