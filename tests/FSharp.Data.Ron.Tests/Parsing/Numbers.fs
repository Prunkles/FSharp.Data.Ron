module FSharp.Data.Ron.Tests.Parsing.Numbers

open System
open Expecto
open FSharp.Data.Ron.Decoding


let integerHex (n: int) =
    let input = $"0x{n:x}"
    let result = Decode.fromString input Decode.int
    Expect.equal result (Ok n) ""

let integerDec (n: int) =
    let input = string n
    let result = Decode.fromString input Decode.int
    Expect.equal result (Ok n) ""

let integerBin (n: int) =
    let input = "0b" + Convert.ToString(n, 2)
    let result = Decode.fromString input Decode.int
    Expect.equal result (Ok n) ""

let integerOct (n: int) =
    let input = "0o" + Convert.ToString(n, 8)
    let result = Decode.fromString input Decode.int
    Expect.equal result (Ok n) ""

[<Tests>]
let tests =
    testList "Parse numbers" [
        testProperty "Parse decimal integer" integerDec
        testProperty "Parse hexadecimal integer" integerHex
        testProperty "Parse binary integer" integerBin
        testProperty "Parse octal integer" integerOct
    ]