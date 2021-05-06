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

let expectFloat source expected msg = expectValue source (RonValue.Float expected) msg

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
            RonValue.Char 'a', RonValue.Integer 1
            RonValue.Char 'b', RonValue.Float 2.0
        ]
        expectValue "{ 'a': 1, 'b': 2.0 }" (RonValue.Map map) ""
    }
    test "number" {
        // integer
        expectValue "42" (RonValue.Integer 42) ""
        expectValue "0x4f" (RonValue.Integer 0x4f) ""
        expectValue "0o47" (RonValue.Integer 0o47) ""
        expectValue "0b101" (RonValue.Integer 0b101) ""
        
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
        
        // float invalid exponent
        for s, el, es in allTriplets [""; "+"; "-"] ["e"; "E"] [""; "+"; "-"] do
            expectError $"{s}3.{el}{es}4" ""
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
            RonValue.Integer 1
            RonValue.Float 2.0
        ]
        expectValue "[1, 2.0]" (RonValue.List vs) "list"
    }
]
