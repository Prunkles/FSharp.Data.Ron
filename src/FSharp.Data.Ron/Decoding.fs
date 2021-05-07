// Inspired by Thoth.Json (https://github.com/thoth-org/Thoth.Json)

module FSharp.Data.Ron.Decoding

[<RequireQualifiedAccess>]
type DecodeError =
    | Parse of string
    | Decode of string
    | Aggregate of string * DecodeError list
    member this.WrapAggregate(msg) = DecodeError.Aggregate(msg, [this])

let private resultMapErrorAggregate msg = Result.mapError (fun (err: DecodeError) -> err.WrapAggregate(msg))

type Decoder<'a> = RonValue -> Result<'a, DecodeError>


module Decoder =
    
    let retn x : Decoder<'a> = fun _ -> Ok x
    
    let errorDecode msg : Decoder<_> = fun _ -> Error (DecodeError.Decode msg)
    
    let bind (binder: 'a -> Decoder<'b>) decoder =
        fun value -> Result.bind (fun r -> (binder r) value) (decoder value)
    
    let map mapping decoder = bind (mapping >> retn) decoder


type DecoderBuilder() =
    member _.Return(x) = Decoder.retn x
    member _.Bind(x, f) = Decoder.bind f x
    member _.ReturnFrom(x: Decoder<'a>) = x
    member _.MergeSources(d1: Decoder<'a>, d2: Decoder<'b>): Decoder<'a * 'b> =
        fun values ->
            let r1 = d1 values
            let r2 = d2 values
            match r1, r2 with
            | Ok x1, Ok x2 -> Ok (x1, x2)
            | (Error err, Ok _) | (Ok _, Error err) -> Error err
            | Error err1, Error err2 -> Error (DecodeError.Aggregate ("Merge failure", [err1; err2]))
    
    member _.ReturnFrom(x: Result<'a, _>): Decoder<'a> = fun _ -> x
    member _.ReturnFrom(x: DecodeError): Decoder<'a> = fun _ -> Error x

[<AutoOpen>]
module DecoderBuilderImpl =
    let decoder = DecoderBuilder()

[<AutoOpen>]
module private ConvertAliases =
    let inline asString a = string a
    let inline asFloat a = float a
    let inline asFloat32 a = float32 a
    let inline asInt a = int a
    let inline asUInt a = uint a
    let inline asInt64 a = int64 a
    let inline asUInt64 a = uint64 a
    let inline asInt16 a = int16 a
    let inline asUInt16 a = uint16 a
    let inline asInt8 a = int8 a
    let inline asUInt8 a = uint8 a

[<RequireQualifiedAccess>]
module Decode =
    
    let fromString input (decoder: Decoder<_>) =
        match Parsing.parseFile input with
        | Error err -> Error (DecodeError.Parse err)
        | Ok { Value = value } ->
            let result = decoder value
            result
    
    let string: Decoder<string> = function RonValue.String s -> Ok s | _ -> Error (DecodeError.Decode "Not a string")
    
    let float: Decoder<float> = function
        | RonValue.Number (RonNumber.Float f) -> Ok f
        | RonValue.Number (RonNumber.Signed i) -> Ok (float i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (float u)
        | _ -> Error (DecodeError.Decode "Not a float")
    
    let float32: Decoder<float32> = function
        | RonValue.Number (RonNumber.Float f) -> Ok (float32 f)
        | RonValue.Number (RonNumber.Signed i) -> Ok (float32 i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (float32 u)
        | _ -> Error (DecodeError.Decode "Not a float")
    
    let int64: Decoder<int64> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok i
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asInt64 u)
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let uint64: Decoder<uint64> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asUInt64 i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok u
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let int: Decoder<int> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asInt i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asInt u)
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let uint: Decoder<uint> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asUInt i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asUInt u)
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let int16: Decoder<int16> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asInt16 i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asInt16 u)
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let uint16: Decoder<uint16> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asUInt16 i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asUInt16 u)
        | _ -> Error (DecodeError.Decode "Not an integer")

    let int8: Decoder<int8> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asInt8 i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asInt8 u)
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let uint8: Decoder<uint8> = function
        | RonValue.Number (RonNumber.Signed i) -> Ok (asUInt8 i)
        | RonValue.Number (RonNumber.Unsigned u) -> Ok (asUInt8 u)
        | _ -> Error (DecodeError.Decode "Not an integer")
    
    let list (elementDecoder: Decoder<'a>) : Decoder<'a list> = function
        | RonValue.List vs ->
            (vs, Ok [])
            ||> Seq.foldBack (fun v r ->
                match r with
                | Error err -> Error err
                | Ok es ->
                    match elementDecoder v with
                    | Error err -> Error (DecodeError.Aggregate ("Failed decode list", [err]))
                    | Ok e -> Ok (e :: es)
            )
        | _ -> Error (DecodeError.Decode  "Not a list")
    
    let map (keyDecoder: Decoder<'k>) (valueDecoder: Decoder<'v>) = function
        | RonValue.Map m ->
            (Ok Map.empty, m)
            ||> Map.fold (fun r kv vv ->
                match r with
                | Error err -> Error err
                | Ok m ->
                    let kr = keyDecoder kv
                    let vr = valueDecoder vv
                    match kr, vr with
                    | Ok k, Ok v ->
                        if m |> Map.containsKey k then
                            Error (DecodeError.Decode $"Key '{k}' occurs more than once")
                        else
                            Ok (m |> Map.add k v)
                    | Error kerr, _ -> Error (kerr.WrapAggregate("Key decode error"))
                    | _, Error verr -> Error (verr.WrapAggregate("Value decode error"))
            )
        | _ -> Error (DecodeError.Decode "Not a map")
    
    let field name (decoder: Decoder<'a>) : Decoder<'a> = function
        | RonValue.AnyStruct (RonStruct.StructNamed (_, content)) ->
            let r = content |> Seq.tryPick (fun (name', value') -> if name' = name then Some value' else None)
            match r with
            | None -> Error (DecodeError.Decode $"No field {name}")
            | Some v ->
                let r = decoder v
                match r with
                | Error err -> Error (err.WrapAggregate($"Failed decode '{name}' field"))
                | Ok x -> Ok x
        | _ -> Error (DecodeError.Decode "Not contains fields")
    
    let item idx (decoder: Decoder<'a>) : Decoder<'a> = function
        | RonValue.AnyStruct (RonStruct.StructTuple (_, items)) ->
            items |> List.tryItem idx
            |> function
                | Some item -> decoder item |> resultMapErrorAggregate $"Failed decode {idx}'nth item"
                | None -> Error (DecodeError.Decode $"Has no {idx}'nth item")
        | _ -> Error (DecodeError.Decode "Has no items")
    
    let unit : Decoder<unit> = function
        | RonValue.AnyStruct RonStruct.Unit -> Ok ()
        | _ -> Error (DecodeError.Decode "Not a unit")
    
    type IFieldGetter =
        abstract Field: string -> Decoder<'f> -> Result<'f, DecodeError>
    
    let record (builder: IFieldGetter -> Result<'r, DecodeError>) : Decoder<'r> = function
        | RonValue.AnyStruct (RonStruct.StructNamed (_, _)) as rvalue ->
            let getField (name: string) (decoder: Decoder<'f>) : Result<'f, DecodeError> =
                let r = rvalue |> field name decoder
                r
            let getter =
                { new IFieldGetter with
                    member _.Field name decoder = getField name decoder }
            builder getter |> resultMapErrorAggregate "Failed decode record"
        | _ -> Error (DecodeError.Decode "Not a record")
    
    type ITupleGetter =
        abstract Item: int -> Decoder<'a> -> Result<'a, DecodeError>
    
    let tuple (builder: ITupleGetter -> Result<'t, DecodeError>) : Decoder<'t> = function
        | RonValue.AnyStruct (RonStruct.Tuple items) ->
            let getItem i decoder =
                match items |> List.tryItem i with
                | None -> Error (DecodeError.Decode $"Tuple has no {i}'th item")
                | Some item -> decoder item |> resultMapErrorAggregate $"Failed decode {i}'nth item"
            let getter = { new ITupleGetter with member _.Item i decoder = getItem i decoder }
            builder getter |> resultMapErrorAggregate "Failed decode tuple"
        | _ -> Error (DecodeError.Decode "Not a tuple")
    
//    let withTag (tag: string) decoder = function
//        | RonValue.AnyStruct (RonStruct.GetTag (Equals tag)) as rvalue -> decoder rvalue
//        | _ -> Error (DecodeError.Decode "Specified case name not matches")
    
    let tag : Decoder<string> = function
        | RonValue.AnyStruct (RonStruct.GetTag tag) -> Ok tag
        | _ -> Error (DecodeError.Decode "Has no tag")
    
    /// Alias to `Decode.item 0`
    let single innerDecoder = item 0 innerDecoder
    
    let option (innerDecoder: Decoder<'a>) : Decoder<'a option> = function
        | RonValue.AnyStruct (RonStruct.Tagged ("None", false)) ->
            Ok None
        | RonValue.AnyStruct (RonStruct.Unnamed (Some "Some", [inner])) ->
            innerDecoder inner |> Result.map Some
        | _ -> Error (DecodeError.Decode "Not an option")
