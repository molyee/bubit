namespace fsext

open System.Runtime.CompilerServices
open LanguagePrimitives

module System =

    [<Extention>]
    type System.Int32 with
        member x.numbits () =
            let mutable i = x
            i <- i - ((i >>> 1) &&& 0x55555555);
            i <- (i &&& 0x33333333) + ((i >>> 2) &&& 0x33333333);
            (((i + (i >>> 4)) &&& 0x0F0F0F0F) * 0x01010101) >>> 24;

    [<Extention>]
    type System.UInt64 with
        member x.numbits () =
            let mutable i = x
            i <- i - ((i >>> 1) &&& 0x5555555555555555UL);
            i <- (i &&& 0x3333333333333333UL) + ((i >>> 2) &&& 0x3333333333333333UL);
            (((i + (i >>> 4)) &&& 0x0f0f0f0f0f0f0f0fUL) * 0x0101010101010101UL) >>> 56;

    [<Extention>]
    type System.Enum with
        static member toSeq<'a when 'a:enum<int>> () =
            let values = System.Enum.GetValues (typeof<'a>)
            [values.GetLowerBound 0 .. values.GetUpperBound 0]
            |> Seq.map (fun x -> EnumOfValue<int,'a>(x))