// https://github.com/ron-rs/ron/blob/ee1aacba02128935f8ad0cae63102a5842dff47b/tests/value.rs

module FSharp.Data.Ron.Tests.Parsing.Values

open Expecto
open FSharp.Data.Ron
open FSharp.Data.Ron.Tests.Utils

let testValue input expected message =
    Expect.equal (Parsing.parseValue input) (Ok expected) message

[<Tests>]
let tests = testList "Values" [
    test "bool" {
        testValue "true" (RonValue.Boolean true) ""
        testValue "false" (RonValue.Boolean false) ""
    }
    test "char" {
        testValue "'a'" (RonValue.Char 'a') ""
    }
    test "map" {
        let map = Map.ofSeq [
            RonValue.Char 'a', RonValue.Integer 1
            RonValue.Char 'b', RonValue.Float 2.0
        ]
        testValue "{ 'a': 1, 'b': 2.0 }" (RonValue.Map map) ""
    }
    test "number" {
        testValue "42" (RonValue.Integer 42) ""
        testValue "3.1415" (RonValue.Float 3.1415) ""
    }
    test "string" {
        let normal = "\"String\""
        testValue normal (RonValue.String "String") "normal"
        
        let raw = "r\"Raw String\""
        testValue raw (RonValue.String "Raw String") "raw"
        
        let rawHashes = "r#\"Raw String\"#"
        testValue rawHashes (RonValue.String "Raw String") "raw hashes"
        
        let rawEscaped = "r##\"Contains \"#\"##"
        testValue rawEscaped (RonValue.String "Contains \"#") "raw escaped"
        
        let rawMultiline = "r\"Multi\nLine\""
        testValue rawMultiline (RonValue.String "Multi\nLine") "raw multiline"
    }
    test "list" {
        let vs = [
            RonValue.Integer 1
            RonValue.Float 2.0
        ]
        testValue "[1, 2.0]" (RonValue.List vs) "list"
    }
]
