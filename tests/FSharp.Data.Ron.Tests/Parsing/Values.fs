// https://github.com/ron-rs/ron/blob/ee1aacba02128935f8ad0cae63102a5842dff47b/tests/value.rs

module FSharp.Data.Ron.Tests.Parsing.Values

open Expecto
open FSharp.Data.Ron
open FSharp.Data.Ron.Tests.Utils

let expectValue input expected message =
    Expect.equal (Parsing.parseValue input) (Ok expected) message

let expectError input message =
    Expect.isError (Parsing.parseValue input) message

let allTriplets s1 s2 s3 =
    seq { for e1 in s1 do for e2 in s2 do for e3 in s3 do yield (e1, e2, e3) }

let expectFloat source expected msg = expectValue source (RonValue.Number (RonNumber.Float expected)) msg

let expectFloatFromString source msg =
    let expected = float source
    expectFloat source expected msg

[<Tests>]
let tests = testList "Values" [
    test "bool" {
        expectValue "true" (RonValue.Boolean true) ""
        expectValue "false" (RonValue.Boolean false) ""
    }
    test "char" {
        expectValue "'a'" (RonValue.Char 'a') ""
    }
    test "map" {
        let map = Map.ofSeq [
            RonValue.Char 'a', RonValue.Number (RonNumber.Unsigned 1UL)
            RonValue.Char 'b', RonValue.Number (RonNumber.Float 2.0)
        ]
        expectValue "{ 'a': 1, 'b': 2.0 }" (RonValue.Map map) ""
    }
    test "number" {
        // integer
        expectValue "42" (RonValue.Number (RonNumber.Unsigned 42UL)) ""
        expectValue "0x4f" (RonValue.Number (RonNumber.Unsigned 0x4fUL)) ""
        expectValue "0o75" (RonValue.Number (RonNumber.Unsigned 0o75UL)) ""
        expectValue "0b101" (RonValue.Number (RonNumber.Unsigned 0b101UL)) ""
        
        expectValue "-42" (RonValue.Number (RonNumber.Signed -42L)) ""
        expectValue "-0x4f" (RonValue.Number (RonNumber.Signed -0x4fL)) ""
        expectValue "-0o75" (RonValue.Number (RonNumber.Signed -0o75L)) ""
        expectValue "-0b101" (RonValue.Number (RonNumber.Signed -0b101L)) ""
        
        // float regular
        for s in [""; "+"; "-"] do
            expectFloatFromString $"{s}3.1415" ""
        
        // float regular exp
        for s, el, es in allTriplets [""; "+"; "-"] ["e"; "E"] [""; "+"; "-"] do
            expectFloatFromString $"{s}3.1415{el}{es}4" ""
        
        // float no integer part
        for s in [""; "+"; "-"] do
            expectFloatFromString $"{s}.31415" ""
        
        // float no integer part with exp
        for s, el, es in allTriplets [""; "+"; "-"] ["e"; "E"] [""; "+"; "-"] do
            expectFloatFromString $"{s}.1415{el}{es}3" ""
        
        // float only integer and exp
        for s, el, es in allTriplets [""; "+"; "-"] ["e"; "E"] [""; "+"; "-"] do
            expectFloatFromString $"{s}314{el}{es}2" ""
        
        // float only integer, dot and exp
        for s, el, es in allTriplets [""; "+"; "-"] ["e"; "E"] [""; "+"; "-"] do
            expectFloatFromString $"{s}314.{el}{es}2" ""
        
        // float invalid exponent
        for s, el, es in allTriplets [""; "+"; "-"] ["e"; "E"] [""; "+"; "-"] do
            expectError $"{s}.{el}{es}4" ""
    }
    test "string" {
        let normal = "\"String\""
        expectValue normal (RonValue.String "String") "normal"
        
        let raw = "r\"Raw String\""
        expectValue raw (RonValue.String "Raw String") "raw"
        
        let rawHashes = "r#\"Raw String\"#"
        expectValue rawHashes (RonValue.String "Raw String") "raw hashes"
        
        let rawEscaped = "r##\"Contains \"#\"##"
        expectValue rawEscaped (RonValue.String "Contains \"#") "raw escaped"
        
        let rawMultiline = "r\"Multi\nLine\""
        expectValue rawMultiline (RonValue.String "Multi\nLine") "raw multiline"
    }
    test "list" {
        let vs = [
            RonValue.Number (RonNumber.Unsigned 1UL)
            RonValue.Number (RonNumber.Float 2.0)
        ]
        expectValue "[1, 2.0]" (RonValue.List vs) "list"
    }
]
