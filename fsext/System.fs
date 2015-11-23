namespace fsext

open System
open System.Runtime.CompilerServices
open LanguagePrimitives

module System =

    let enum64<'a when 'a:enum<uint64>> (value:uint64) : 'a = EnumOfValue value

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
        static member toList<'a when 'a:enum<int>> () =
            Enum.GetValues (typeof<'a>) :?> 'a[] |> List.ofArray