module FSharp.Data.Ron.Tests.Parsing.Floats

open System
open Expecto
open FSharp.Data.Ron.Decoding

module Expect =
    
    let equalFloats actual expected message =
        let (|IsNaN|_|) = function f when Double.IsNaN(f) -> Some () | _ -> None
        match actual, expected with
        | IsNaN, IsNaN ->
            let x = "NaN (fictive)"
            Expect.equal x x message
        | actual, expected -> Expect.equal actual expected message

    let equalResult equalOk actual expected message =
        match actual, expected with
        | Ok actual, Ok expected -> equalOk actual expected message
        | actual, expected -> Expect.equal actual expected message


[<Tests>]
let tests = testList "Parse floats" [
    test "Parse 'inf'" {
        let input = "inf"
        Expect.equalResult Expect.equalFloats (Decode.fromString input Decode.float) (Ok infinity) ""
    }
    test "Parse '+inf'" {
        let input = "+inf"
        Expect.equalResult Expect.equalFloats (Decode.fromString input Decode.float) (Ok infinity) ""
    }
    test "Parse '-inf'" {
        let input = "-inf"
        Expect.equalResult Expect.equalFloats (Decode.fromString input Decode.float) (Ok Double.NegativeInfinity) ""
    }
    test "Parse 'NaN'" {
        let input = "NaN"
        Expect.equalResult Expect.equalFloats (Decode.fromString input Decode.float) (Ok nan) "'NaN' isn't parsed"
    }
//    testProperty "Parse '0.0' format float" <| fun (f: float) ->
//        let input =
//            match f with
//            | f when Double.IsPositiveInfinity(f) -> "inf"
//            | f when Double.IsNegativeInfinity(f) -> "-inf"
//            | f -> f.ToString("f400", System.Globalization.CultureInfo.InvariantCulture) // No format with f400 
//        Expect.equalResult Expect.equalFloats (Decode.fromString input Decode.float) (Ok f) ""
    testProperty "Parse '0.0E0' format float" <| fun (f: float) ->
        let input =
            match f with
            | f when Double.IsPositiveInfinity(f) -> "inf"
            | f when Double.IsNegativeInfinity(f) -> "-inf"
            | f -> f.ToString("E17", System.Globalization.CultureInfo.InvariantCulture)
        Expect.equalResult Expect.equalFloats (Decode.fromString input Decode.float) (Ok f) ""
]
