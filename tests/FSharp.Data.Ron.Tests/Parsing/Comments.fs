module FSharp.Data.Ron.Tests.Parsing.Comments

open Expecto
open FSharp.Data.Ron
open FSharp.Data.Ron.Decoding
open FSharp.Data.Ron.Tests

// https://github.com/ron-rs/ron/blob/ee1aacba02128935f8ad0cae63102a5842dff47b/tests/comments.rs


[<Tests>]
let tests = testList "Comment parsing" [
    test "Parse simple comment" {
        let input = """/*
 * We got a hexadecimal number here!
 *
 */0x507"""
        Expect.equal (Decode.fromString input Decode.int) (Ok 0x507) "Failed parse simple comment"
    }
    test "Parse nested comment" {
        let input = """/*
        /* quite * some * nesting * going * on * /* here /* (yeah, maybe a bit too much) */ */ */
    */
    // The actual value comes.. /*
    // very soon, these are just checks that */
    // multi-line comments don't trigger in line comments /*
"THE VALUE" /* This is the value /* :) */ */
    """
        Expect.equal (Decode.fromString input Decode.string) (Ok "THE VALUE") "Failed parse nested comment"
    }
    test "Parse unclosed comment" {
        let input = """/*
        /* quite * some * nesting * going * on * /* here /* (yeah, maybe a bit too much) */ */ */
    */
    // The actual value comes.. /*
    // very soon, these are just checks that */
    // multi-line comments don't trigger in line comments /*
/* Unfortunately, this comment won't get closed :(
"THE VALUE (which is invalid)"
"""
        let result = Decode.fromString input (fun _ -> failtest "Comment should never be parsed successfully")
        Expect.isError result "" // TODO: Specify error
    }
]