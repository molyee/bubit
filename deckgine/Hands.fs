namespace deckgine

open System
open Cards

type Rank =
    HighCard = 1
    | OnePair = 2
    | TwoPairs = 3
    | ThreeOfKind = 4
    | Straight = 5
    | Flush = 6
    | FullHouse = 7
    | FourOfKind = 8
    | StraightFlush = 9
    | RoyalFlush = 10

type Hand = { rank:Rank; cards:Card }

module Hands =
    let best (hands:Hand list) = ()
    let get (rank:Rank) (cards:Card) =
        match rank with
        | Rank.Flush | Rank.StraightFlush | Rank.RoyalFlush ->
            Cards.flush cards
            |> Seq.filter (fun (suit,cards) -> Cards.count cards > 4)
            |> Seq.map (fun (suit,cards) -> ())
        | _ -> raise (Exception (sprintf "Unknown hand rank %A" rank))