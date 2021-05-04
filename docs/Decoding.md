# Decoding

> The decoding principle almost entirely inspired by [Thoth.Json](https://github.com/thoth-org/Thoth.Json),
> so basically the [Thoth.Json documentation](https://thoth-org.github.io/Thoth.Json/#Decoder)
> mostly applies here.

## Single value

```f#
open FSharp.Data.Ron.Decoding
```

```f#
> Decode.fromString "32" Decode.int
val it : Result<int, DecodeError> = Ok 32

> Decode.fromString "\"foo\"" Decode.string
val it : Result<int, DecodeError> = Ok "foo"
```

## Builder style

```f#
type Vector2 = { X: int; Y: int }

let decode = decoder {
    let! x = Decode.field "x" Decode.int
    and! y = Decode.field "y" Decide.int
    return { X = x; Y = y }
}
```

```f#
type Shape =
    | Square of float * float
    | Circle of float

let decode = decoder {
    match! Decode.tag with
    | "Square" ->
        let! w = Decode.item 0 Decode.float
        and! h = Decode.item 1 Decode.float
        return Square (w, h)
    | "Circle" ->
        let! r = Decode.single Decode.float
        return Circle r
    | _ ->
        return! Decoder.errorDecode "Invalid tag"
}
```

## More samples

You can also check more samples in tests [here](https://github.com/Prunkles/FSharp.Data.Ron/tree/main/tests/FSharp.Data.Ron.Tests/Samples).
