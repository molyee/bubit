namespace deckgine

open System
open fsext.System

[<Flags>]
type Card = 
    empty = 0x0UL
    | H2 = 0x1UL | D2 = 0x2UL | C2 = 0x4UL | S2 = 0x8UL
    | H3 = 0x10UL | D3 = 0x20UL | C3 = 0x40UL | S3 = 0x80UL
    | H4 = 0x100UL | D4 = 0x200UL | C4 = 0x400UL | S4 = 0x800UL
    | H5 = 0x1000UL | D5 = 0x2000UL | C5 = 0x4000UL | S5 = 0x8000UL
    | H6 = 0x10000UL | D6 = 0x20000UL | C6 = 0x40000UL | S6 = 0x80000UL
    | H7 = 0x100000UL | D7 = 0x200000UL | C7 = 0x400000UL | S7 = 0x800000UL
    | H8 = 0x1000000UL | D8 = 0x2000000UL | C8 = 0x4000000UL| S8 = 0x8000000UL
    | H9 = 0x10000000UL | D9 = 0x20000000UL | C9 = 0x40000000UL | S9 = 0x80000000UL
    | HT = 0x100000000UL | DT = 0x200000000UL | CT = 0x400000000UL | ST = 0x800000000UL
    | HJ = 0x1000000000UL | DJ = 0x2000000000UL | CJ = 0x4000000000UL | SJ = 0x8000000000UL
    | HQ = 0x10000000000UL | DQ = 0x20000000000UL | CQ = 0x40000000000UL | SQ = 0x80000000000UL
    | HK = 0x100000000000UL | DK = 0x200000000000UL | CK = 0x400000000000UL | SK = 0x800000000000UL
    | HA = 0x1000000000000UL | DA = 0x2000000000000UL | CA = 0x4000000000000UL | SA = 0x8000000000000UL

[<Flags>]
type Suit = None = 0 | Hearts = 1 | Diams = 2 | Clubs = 4 | Spades = 8

type Kind =
    Two = 0 | Three = 1 | Four = 2 | Five = 3
    | Six = 4 | Seven = 5 | Eight = 6 | Nine = 7
    | Ten = 8 | Jack = 9 | Queen = 10
    | King = 11 | Ace = 12

exception PickErr of Card * Card
exception JoinErr of Card * Card
exception SuitErr of Card * Suit
exception NotEnoughErr of Card * int

type CardInfo = { card:Card; kind:Kind; suit:Suit }

module Cards =
    let make (source:uint64) : Card = enum64<Card> source
    let private hbit = make 0x1111111111111UL
    let private dbit = make 0x2222222222222UL
    let private cbit = make 0x4444444444444UL
    let private sbit = make 0x8888888888888UL
    let private cardsOfKind = Enum.toList<Kind>() |> List.map (fun k -> (k,make (0xFUL <<< (4 * (int k))))) |> List.rev
    let private suits = Enum.toList<Suit>() |> List.tail
    let private allCards = Enum.GetValues (typeof<Card>) :?> Card[] |> List.ofArray |> List.tail |> List.rev

    let private extract (card:Card) =
        let i = uint64 card
        if not (i &&& (i - 1UL) = 0UL) then raise (Exception(sprintf "Maybe multiple cards received. Not pow of 2 - %A (%d)" card i))
        let kind = int (Math.Floor (Math.Log (float i, 16.)))
        let suit = int ((i >>> (4 * kind)) % 16UL)
        {card=card; kind=enum<Kind> kind; suit=enum<Suit> suit}
        
    let private extracted = allCards |> List.map extract

    let str (cards:Card) : string =
        let str = cards.ToString()
        match str.[1] with
        | 'T' -> string ([|"10", str.[0]|])
        | x -> string ([|x, str.[0]|])
    
    let inline isEmpty (set:Card) = set = Card.empty
    
    let inline private containsAll (cards:Card) (set:Card) = set &&& cards = cards

    let existIn (cards:Card) (card:Card) = containsAll card cards

    let ofKind (kind:Kind) (cards:Card) =
        cardsOfKind |> Seq.filter (fun (k,c) -> k = kind) |> List.ofSeq

    let ofSuit (suit:Suit) (cards:Card) = 
        match suit with
        | Suit.Hearts -> cards &&& hbit
        | Suit.Diams -> cards &&& dbit
        | Suit.Clubs -> cards &&& cbit
        | Suit.Spades -> cards &&& sbit
        | _ -> raise <| SuitErr (cards,suit)

    let count (cards:Card) = int((uint64 cards).numbits())
    
    let top (cards:Card) = allCards |> List.find (existIn cards)

    let less (cards:Card) = allCards |> List.findBack (existIn cards)

    let toList (cards:Card) = allCards |> List.filter (existIn cards)

    let flush (cards:Card) =
        suits
        |> Seq.map (fun suit -> (suit,ofSuit suit cards))
        |> Seq.filter (fun (suit,set) -> not (isEmpty set))
        |> Map.ofSeq

    let split (cards:Card) =
        cardsOfKind
        |> Seq.map (fun (kind,mask) -> (kind,mask &&& cards))
        |> Seq.filter (fun (kind,set) -> not (isEmpty set))
        |> Map.ofSeq

    let pick (cards:Card) (set:Card) =
        match containsAll cards set with
        | true -> set ^^^ cards
        | _ -> raise <| PickErr (cards, set)

    let join (cards:Card) (set:Card) =
        match cards &&& set with
        | Card.empty -> cards ||| set
        | _ -> raise <| JoinErr (cards, set)
   
    let unzip (cards:Card) =
        extracted |> List.filter (fun info -> existIn cards info.card)

    let zip (cards:CardInfo seq) =
        cards |> Seq.fold (fun cs info -> join cs info.card) Card.empty
    
    let best (num:int) (cards:Card) =
        let rec best' (num:int) (count:int) (cards:Card) =
            match count with
            | c when c > num ->
                let h = top cards
                h :: best' (num-1) (c-1) (pick cards h)
            | c when c = num -> toList cards
            | _ -> raise <| NotEnoughErr (cards,num)
        best' num (count cards) cards

    let shuffle (cards:Card) : (Card seq * int) =
        let random = Random(DateTime.UtcNow.Millisecond)
        cards
        |> toList
        |> List.map (fun card -> (random.Next(), card))
        |> List.sortBy (fun (i,card) -> i)
        |> List.map (fun (i, card) -> card)
        |> fun l -> (Seq.ofList l, List.length l)
