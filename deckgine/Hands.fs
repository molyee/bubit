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

type Hand = {cards:CardInfo[]; rank:Rank}

exception UnknownRankErr of Rank
exception EmptyHandsListErr

module Hands =
    
    let private make (rank:Rank) (cards:CardInfo list) =
        { cards=cards |> Array.ofList; rank=rank }

    let private getStraight (cards:CardInfo list) =
        let rec getStraight' (straight:CardInfo list) (cards:CardInfo list) =
            match straight.Length with
            | 5 -> straight
            | _ ->
                match cards with
                | [] -> []
                | info::t ->
                    match straight with
                    | [] -> getStraight' (info::[]) t
                    | sinfo::tail as l ->
                        match (int sinfo.kind) - (int info.kind) with
                        | 1 -> getStraight' (info::l) t
                        | 0 -> getStraight' l t
                        | x when x > 1 -> getStraight' (info::[]) t
                        | _ -> raise <| InvalidProgramException("Maybe not sorted cards received")
        let sorted = cards |> List.sortByDescending (fun ci -> uint64 ci.card)
        getStraight' [] sorted

    let get (cards:Card) =
        let getNoFlush (cards:Card) =
            let split = Cards.split cards |> Map.map (fun k cs -> Cards.unzip cs) |> Map.toList
            let trips = split |> List.tryFind (fun (k,cs) -> cs.Length = 3)
            match trips with
            | Some (k,tps) ->
                let pair = split |> List.tryFind (fun (pk,cs) -> cs.Length >= 2 && not (k = pk))
                match pair with
                | Some (nk,pr) -> make Rank.FullHouse (List.append tps (List.take 2 pr))
                | None ->
                    let straight = getStraight (Cards.unzip cards)
                    match straight with
                    | h::t -> make Rank.Straight straight
                    | [] -> make Rank.ThreeOfKind tps
            | None ->
                let straight = getStraight (Cards.unzip cards)
                match straight with
                | h::t -> make Rank.Straight straight
                | [] ->
                    let pairs = split |> List.filter (fun (k,cs) -> cs.Length = 2)
                    match pairs with
                    | [] -> make Rank.HighCard (Cards.top cards |> Cards.unzip)
                    | [(_,infos)] -> make Rank.OnePair infos
                    | _ ->
                        pairs |> List.sortByDescending (fun (k,cs) -> int k) |> List.take 2
                        |> List.map (fun (k,cs) -> cs) |> List.concat
                        |> make Rank.TwoPairs
        let flushes = cards |> Cards.flush |> Map.filter (fun s cs -> Cards.count cs >= 5) |> Map.map (fun s cs -> Cards.unzip cs) |> Map.toList
        match flushes with
        | [] -> getNoFlush cards
        | [(suit,flush)] ->
            let straighflush = getStraight flush
            match straighflush with
            | [] ->
                let split = Cards.split cards |> Map.map (fun k cs -> Cards.unzip cs) |> Map.toList
                let quads = split |> List.tryFind (fun (k,cs) -> cs.Length >= 4)
                match quads with
                | Some (k,qds) -> make Rank.FourOfKind (qds |> List.take 4)
                | None -> make Rank.Flush flush
            | h::t as l -> 
                let opt = l |> List.tryFind (fun ci -> ci.kind = Kind.Ace)
                match opt with
                | Some ace -> make Rank.RoyalFlush straighflush
                | None -> make Rank.StraightFlush straighflush
        | _ -> raise <| NotImplementedException("Can't select best flush from list yet")
        