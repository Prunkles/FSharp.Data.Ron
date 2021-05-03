module FSharp.Data.Ron.Tests.Parsing.Floats

open System
open Expecto
open FSharp.Data.Ron
open FSharp.Data.Ron.Decoding
open FSharp.Data.Ron.Tests

[<Tests>]
let tests = testList "Parse floats" [
    test "Parse 'inf'" {
        Expect.equal (Decode.fromString "inf" Decode.float) (Ok infinity) ""
    }
    test "Parse '-inf'" {
        Expect.equal (Decode.fromString "-inf" Decode.float) (Ok Double.NegativeInfinity) ""
    }
    test "Parse 'NaN'" {
        Expect.equal (Decode.fromString "NaN" Decode.float) (Ok nan) "" // TODO: NaN inequality
    }
    testProperty "Parse '0.0' float" <| fun (f: float) ->
        let input = f.ToString("f")
        Expect.equal (Decode.fromString input Decode.float) (Ok f) ""
    testProperty "Parse '0.0E0' float" <| fun (f: float) ->
        let input = f.ToString("E")
        Expect.equal (Decode.fromString input Decode.float) (Ok f) ""
//    testProperty "Parse 'x.x' float" <| fun (f: float) ->
//        let input = string f
//        Expect.equal (Parsing.parseValue input) (Ok (RonValue.Float f)) ""
]
