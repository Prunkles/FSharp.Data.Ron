// https://github.com/ron-rs/ron/blob/ee1aacba02128935f8ad0cae63102a5842dff47b/tests/value.rs

module FSharp.Data.Ron.Tests.Parsing.Values

open Expecto
open FSharp.Data.Ron
open FSharp.Data.Ron.Tests.Utils

let expectValue input expected message =
    Expect.equal (Parsing.parseValue input) (Ok expected) message

let expectError input message =
    Expect.isError (Parsing.parseValue input) message


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
        expectValue "3.1415" (RonValue.Float 3.1415) ""
        expectValue "+3.1415" (RonValue.Float 3.1415) ""
        expectValue "-3.1415" (RonValue.Float -3.1415) ""
        
        // float regular exp
        expectValue "0.031415e2" (RonValue.Float 3.1415) ""
        expectValue "0.031415E2" (RonValue.Float 3.1415) ""
        expectValue "+0.031415e2" (RonValue.Float 3.1415) ""
        expectValue "+0.031415E2" (RonValue.Float 3.1415) ""
        expectValue "-0.031415e2" (RonValue.Float -3.1415) ""
        expectValue "-0.031415E2" (RonValue.Float -3.1415) ""
        
        expectValue "0.031415e+2" (RonValue.Float 3.1415) ""
        expectValue "0.031415E+2" (RonValue.Float 3.1415) ""
        expectValue "+0.031415e+2" (RonValue.Float 3.1415) ""
        expectValue "+0.031415E+2" (RonValue.Float 3.1415) ""
        expectValue "-0.031415e+2" (RonValue.Float -3.1415) ""
        expectValue "-0.031415E+2" (RonValue.Float -3.1415) ""
        
        expectValue "314.15e-2" (RonValue.Float 3.1415) ""
        expectValue "314.15E-2" (RonValue.Float 3.1415) ""
        expectValue "+314.15e-2" (RonValue.Float 3.1415) ""
        expectValue "+314.15E-2" (RonValue.Float 3.1415) ""
        expectValue "-314.15e-2" (RonValue.Float -3.1415) ""
        expectValue "-314.15E-2" (RonValue.Float -3.1415) ""
        
        // float no integer part
        expectValue ".31415" (RonValue.Float 0.31415) ""
        expectValue "+.31415" (RonValue.Float 0.31415) ""
        expectValue "-.31415" (RonValue.Float -0.31415) ""
        
        expectValue ".31415e1" (RonValue.Float 3.1415) ""
        expectValue ".31415E1" (RonValue.Float 3.1415) ""
        expectValue "+.31415e1" (RonValue.Float 3.1415) ""
        expectValue "+.31415E1" (RonValue.Float 3.1415) ""
        expectValue "-.31415e1" (RonValue.Float -3.1415) ""
        expectValue "-.31415E1" (RonValue.Float -3.1415) ""
        
        expectValue ".31415e+1" (RonValue.Float 3.1415) ""
        expectValue ".31415E+1" (RonValue.Float 3.1415) ""
        expectValue "+.31415e+1" (RonValue.Float 3.1415) ""
        expectValue "+.31415E+1" (RonValue.Float 3.1415) ""
        expectValue "-.31415e+1" (RonValue.Float -3.1415) ""
        expectValue "-.31415E+1" (RonValue.Float -3.1415) ""
        
        expectValue ".31415e-1" (RonValue.Float 0.031415) ""
        expectValue ".31415E-1" (RonValue.Float 0.031415) ""
        expectValue "+.31415e-1" (RonValue.Float 0.031415) ""
        expectValue "+.31415E-1" (RonValue.Float 0.031415) ""
        expectValue "-.31415e-1" (RonValue.Float -0.031415) ""
        expectValue "-.31415E-1" (RonValue.Float -0.031415) ""
        
        // float only integer and exp
        expectValue "3e2" (RonValue.Float 300.0) ""
        expectValue "3E2" (RonValue.Float 300.0) ""
        expectValue "+3e2" (RonValue.Float 300.0) ""
        expectValue "+3E2" (RonValue.Float 300.0) ""
        expectValue "-3e2" (RonValue.Float -300.0) ""
        expectValue "-3E2" (RonValue.Float -300.0) ""
        
        expectValue "3e+2" (RonValue.Float 300.0) ""
        expectValue "3E+2" (RonValue.Float 300.0) ""
        expectValue "+3e+2" (RonValue.Float 300.0) ""
        expectValue "+3E+2" (RonValue.Float 300.0) ""
        expectValue "-3e+2" (RonValue.Float -300.0) ""
        expectValue "-3E+2" (RonValue.Float -300.0) ""
        
        expectValue "3e-2" (RonValue.Float 0.03) ""
        expectValue "3E-2" (RonValue.Float 0.03) ""
        expectValue "+3e-2" (RonValue.Float 0.03) ""
        expectValue "+3E-2" (RonValue.Float 0.03) ""
        expectValue "-3e-2" (RonValue.Float -0.03) ""
        expectValue "-3E-2" (RonValue.Float -0.03) ""
        
        // float invalid exponent
        expectError "3.e2" ""
        expectError "3.E2" ""
        expectError "3.e2" ""
        expectError "3.E2" ""
        expectError "3.e2" ""
        expectError "3.E2" ""
        
        expectError "+3.e+2" ""
        expectError "+3.E+2" ""
        expectError "+3.e+2" ""
        expectError "+3.E+2" ""
        expectError "+3.e+2" ""
        expectError "+3.E+2" ""
        
        expectError "-3.e-2" ""
        expectError "-3.E-2" ""
        expectError "-3.e-2" ""
        expectError "-3.E-2" ""
        expectError "-3.e-2" ""
        expectError "-3.E-2" ""
        
        expectError ".e2" ""
        expectError ".E2" ""
        expectError ".e2" ""
        expectError ".E2" ""
        expectError ".e2" ""
        expectError ".E2" ""
        
        expectError "+.e+2" ""
        expectError "+.E+2" ""
        expectError "+.e+2" ""
        expectError "+.E+2" ""
        expectError "+.e+2" ""
        expectError "+.E+2" ""
        
        expectError "-.e-2" ""
        expectError "-.E-2" ""
        expectError "-.e-2" ""
        expectError "-.E-2" ""
        expectError "-.e-2" ""
        expectError "-.E-2" ""
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
