// Inspired by Thoth.Json (https://github.com/thoth-org/Thoth.Json)

module FSharp.Data.Ron.Decoding

[<RequireQualifiedAccess>]
type DecodeError =
    | Parse of string
    | Decode of string

type Decoder<'a> = RonValue -> Result<'a, DecodeError>

[<RequireQualifiedAccess>]
module Decode =
    
    let fromString input (decoder: Decoder<_>) =
        match Parsing.parse input with
        | Error err -> Error (DecodeError.Parse err)
        | Ok { Value = value } ->
            let result = decoder value
            result
    
    let string: Decoder<string> = function RonValue.String s -> Ok s | _ -> Error (DecodeError.Decode "Not a string")
    
    let list (elementDecoder: Decoder<'a>) : Decoder<'a list> = function
        | RonValue.List vs ->
            (Ok [], vs)
            ||> Seq.fold (fun s v ->
                match s with
                | Error err -> Error err
                | Ok es ->
                    match elementDecoder v with
                    | Error err -> Error err
                    | Ok e -> Ok (e :: es)
            )
        | _ -> Error (DecodeError.Decode  "Not a list")
    
    let float: Decoder<float> = function RonValue.Float f -> Ok f | _ -> Error (DecodeError.Decode "Not a float")
    
    let int: Decoder<int> = function RonValue.Integer i -> Ok i | _ -> Error (DecodeError.Decode "Not an integer")
