[<AutoOpen>]
module FSharp.Data.Ron.Tests.Utils

open FSharp.Data.Ron

module Parsing =
    let parseValue input =
        match Parsing.parse input with
        | Ok { Value = value } -> Ok value
        | Error err -> Error err
