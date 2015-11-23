namespace deckgine

open System
open Cards

type DeckType = Deck52 | Deck52J2 | Deck36

type Deck = { cards:Card seq; size:int }

exception OutOfDeckErr of Deck * int
exception UknownDeckTypeErr of DeckType

module Decks =

    let take (count:int) (deck:Deck) : (Card list * Deck) =
        let remaining = deck.size - count
        match remaining >= 0 with
        | true -> deck.cards |> Seq.take count |> Seq.toList, { cards = deck.cards; size = deck.size-count }
        | _ -> raise <| OutOfDeckErr (deck,count)
    
    let cat (index:int) (deck:Deck) : Deck =
        match index with
        | x when x < 0 || x > deck.size -> raise <| OutOfDeckErr (deck,x)
        | x -> { cards = Seq.append (Seq.skip x deck.cards) (Seq.take x deck.cards); size = deck.size }
    
    let make (decktype:DeckType) : Deck =
        let cards =
            match decktype with
            | Deck52 -> Cards.make(0xFFFFFFFFFFFFFUL)
            | Deck36 -> Cards.make(0xFFFFFFFFF0000UL)
            | _ -> raise <| UknownDeckTypeErr decktype
        let shuffled = Cards.shuffle cards
        { cards = fst shuffled; size = snd shuffled }

    