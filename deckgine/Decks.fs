namespace deckgine

open System
open System.Collections.Generic

open Cards

type DeckType = Deck52 | Deck52J | Deck36

type Deck = { cards:Card seq; size:int }

module Decks =

    let take (count:int) (deck:Deck) : (Card list * Deck) =
        let remaining = deck.size - count
        match remaining >= 0 with
        | true -> (deck.cards |> Seq.take count |> Seq.toList, { cards = deck.cards; size = deck.size-count })
        | _ -> raise (Exception(sprintf "It's not enough cards in the deck %d < %d" deck.size count))
    
    let cat (index:int) (deck:Deck) : Deck =
        match index with
        | x when x < 0 -> raise (ArgumentOutOfRangeException("Negative cards index"))
        | x when x > deck.size -> raise (ArgumentOutOfRangeException(sprintf "Cards index %d is bigger then deck size %d" x deck.size))
        | x -> 
    
    let make (deck:DeckType) : Deck =
        shuffle (
            match deck with
            | Deck52 -> Cards.make(0xFFFFFFFFFFFFFUL)
            | Deck36 -> Cards.make(0xFFFFFFFFF0000UL)
            | Deck52J -> Cards.make(0x3FFFFFFFFFFFFFUL)
            | _ -> raise (Exception (sprintf "Unknown deck type %A" deck)))

    