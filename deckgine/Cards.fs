namespace deckgine

open System
open System.Collections.Generic
open LanguagePrimitives
open fsext.System

[<FlagsAttribute>]
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

[<FlagsAttribute>]
type Suit = None = 0 | Hearts = 1 | Diams = 2 | Clubs = 4 | Spades = 8

module Cards =
    let make (source:uint64) : Card = EnumOfValue source
    let private hbit = make 0x1111111111111UL
    let private dbit = make 0x2222222222222UL
    let private cbit = make 0x4444444444444UL
    let private sbit = make 0x8888888888888UL

    let str (cards:Card) : string =
        let str = cards.ToString()
        match str.[1] with
        | 'T' -> string ([|"10", str.[0]|])
        | x -> string ([|x, str.[0]|])
    
    let cardsOf (suit:Suit) (cards:Card) = 
        match suit with
        | Suit.Hearts -> cards &&& hbit
        | Suit.Diams -> cards &&& dbit
        | Suit.Clubs -> cards &&& cbit
        | Suit.Spades -> cards &&& sbit
        | _ -> raise (Exception (sprintf "Unknown suit type %A" suit))

    let count (cards:Card) = (EnumToValue cards).numbits()

    let suits (cards:Card) : Suit =
        Enum.GetValues(typeof<Suit>)
        |> Seq.i (fun i -> enum<Suit> i)
        |> Seq.map (fun (s:Suit) -> (s,cardsOf s cards))
        |> Seq.fold (fun suits (s,cards) -> if cards != Card.empty then (s ||| suits) else suits) Suit.None

    let isEmpty (set:Card) = set = Card.empty
    
    let contains (cards:Card) (set:Card) = set &&& cards = cards

    let intersect (set1:Card) (set2:Card) = not (set1 &&& set2 = Card.empty)

    let pick (cards:Card) (set:Card) =
        match contains cards set with
        | true -> set ^^^ cards
        | _ -> raise (InvalidOperationException (sprintf "Some cards '%A' not found in the set '%A'" cards set))

    let join (cards:Card) (set:Card) =
        match intersect cards set with
        | false -> cards ||| set
        | _ -> raise (InvalidOperationException (sprintf "Some cards '%A' found in the set '%A'" cards set))
    
    let shuffle (cards:Card) : (Card seq * int) =
        let random = Random(DateTime.UtcNow.Millisecond)
        let sorted = new SortedList<int, Card>();
        [0..63]
        |> List.map (fun x -> EnumOfValue (1UL <<< x))
        |> List.filter (fun card -> contains card cards)
        |> List.map (fun card -> (random.Next(), card))
        |> List.sortBy (fun (i,card) -> i)
        |> List.map (fun (i, card) -> card)
        |> fun l -> (Seq.ofList l, List.length l)

