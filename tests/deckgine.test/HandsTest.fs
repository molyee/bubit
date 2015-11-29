module deckgine.test.HandsTest

open FsUnit
open Xunit
open deckgine

// Quads checking
type ``Checking hands`` () =
    
    let isHand (rank:Rank) (hand:Card) (cards:Card) =
        let h = Hands.get cards
        h.rank |> should equal rank
        h.cards |> Array.toSeq |> Cards.zip |> should equal hand

    [<Fact>]
    let ``Given As,Ks,Ah,Qs,Js,Ad,Ts,Ac should be a Royal Flush (As..Ts)`` () =
        Card.SA|||Card.SK|||Card.HA|||Card.SQ|||Card.SJ|||Card.DA|||Card.ST|||Card.CA
        |> isHand Rank.RoyalFlush (Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.ST)

    [<Fact>]
    let ``Given Kc,Kd,Qd,Jd,Td,9d,Kh,Ks should be a Straight Flush (Kd..9d)`` () =
        Card.CK|||Card.DK|||Card.DQ|||Card.DJ|||Card.DT|||Card.D9|||Card.HK|||Card.SK
        |> isHand Rank.StraightFlush (Card.DK|||Card.DQ|||Card.DJ|||Card.DT|||Card.D9)

    [<Fact>]
    let ``Given Ah,Ac,Ad,As,5c,4c,3c,2c should be a Straight Flush (5c..Ac)`` () =
        Card.HA|||Card.CA|||Card.DA|||Card.SA|||Card.C5|||Card.C4|||Card.C3|||Card.C2
        |> isHand Rank.StraightFlush (Card.C5|||Card.C4|||Card.C3|||Card.C2|||Card.CA)

    [<Fact>]
    let ``Given Ah,Ac,Ad,As,5c,4c,3c,2c should be a Four of Kind (A)`` () =
        Card.HA|||Card.CA|||Card.DA|||Card.SA|||Card.C6|||Card.C4|||Card.C3|||Card.C2
        |> isHand Rank.FourOfKind (Card.HA|||Card.CA|||Card.DA|||Card.SA)

    [<Fact>]
    let ``Given As,Ks,Ah,Qs,Js,Td,Ad,Qc should be a Full House (A & Q)`` () =
        Card.SA|||Card.SK|||Card.HA|||Card.SQ|||Card.SJ|||Card.DT|||Card.DA|||Card.CQ
        |> isHand Rank.FullHouse (Card.SA|||Card.HA|||Card.DA|||Card.SQ|||Card.CQ)

    [<Fact>]
    let ``Given As,Ks,Qs,Js,Td,Ad,Qc,5s should be a Flash of Spades (A..)`` () =
        Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.CA|||Card.DA|||Card.CT|||Card.S5
        |> isHand Rank.Flush (Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.S5)

    [<Fact>]
    let ``Given As,Ks,Qs,Js,Td,Ad,Qc,5h should be a Straight (A..T)`` () =
        Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.DT|||Card.DA|||Card.CT|||Card.H5
        |> isHand Rank.Straight (Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.CT)

    [<Fact>]
    let ``Given As,Ks,Qs,Js,Qh,9d,Qc,5h should be a Three of Kind (Q)`` () =
        Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.HQ|||Card.D9|||Card.CQ|||Card.H5
        |> isHand Rank.ThreeOfKind (Card.SQ|||Card.HQ|||Card.CQ)

    [<Fact>]
    let ``Given As,Ks,Qs,Js,Qh,Jd,5c,5h should be a Two Pairs (Q & J)`` () =
        Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.HQ|||Card.DJ|||Card.C5|||Card.H5
        |> isHand Rank.TwoPairs (Card.SQ|||Card.HQ|||Card.SJ|||Card.DJ)

    [<Fact>]
    let ``Given As,Ks,Qs,Js,9h,Jd,5c,4h should be an One Pair (J)`` () =
        Card.SA|||Card.SK|||Card.SQ|||Card.SJ|||Card.H9|||Card.DJ|||Card.C5|||Card.H4
        |> isHand Rank.OnePair (Card.SJ|||Card.DJ)

    [<Fact>]
    let ``Given Ks,Qs,Js,9h,8d,5c,4h,2s should be an One Pair (J)`` () =
        Card.SK|||Card.SQ|||Card.SJ|||Card.H9|||Card.D8|||Card.C5|||Card.H4|||Card.S2
        |> isHand Rank.HighCard (Card.SK)