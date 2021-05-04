module FSharp.Data.Ron.Tests.Samples.Sample1

open Expecto
open FSharp.Data.Ron
open FSharp.Data.Ron.Decoding

[<RequireQualifiedAccess>]
type Arc =
    | Circle of float
    | Arc of r: float * degrees: float

type Rect = { w: float; h: float }

[<RequireQualifiedAccess>]
type Shape =
    | Point
    | Square of float
    | Rect of w: float * h: float
    | Circle of Arc
    | Named of string

let input = """
[
    Point,
    Square(12),
    Rect(w: 5., h: .5),
    Circle(Circle(12)),
    Circle(Arc(r: 1.6e+2, degrees: 95.47)),
    Named("Sharp shape"),
]
"""

let expected =
    [ Shape.Point
      Shape.Square 12.0
      Shape.Rect (5.0, 0.5)
      Shape.Circle (Arc.Circle 12.0)
      Shape.Circle (Arc.Arc (1.6e+2, 95.47))
      Shape.Named "Sharp shape" ]


let decodeArc =  decoder {
    match! Decode.tag with
    | "Circle" ->
        let! r = Decode.item 0 Decode.float
        return Arc.Circle r
    | "Arc" ->
        let! r = Decode.field "r" Decode.float
        and! degrees = Decode.field "degrees" Decode.float
        return Arc.Arc (r, degrees)
    | _ -> return! Decoder.errorDecode "Invalid tag"
}

let decodeShape = decoder {
    match! Decode.tag with
    | "Point" ->
        // do! Decode.empty
        return Shape.Point
    | "Square" ->
        let! a = Decode.item 0 Decode.float
        return Shape.Square a
    | "Rect" ->
        let! w = Decode.field "w" Decode.float
        and! h = Decode.field "h" Decode.float
        return Shape.Rect (w, h)
    | "Circle" ->
        let! arc = Decode.item 0 decodeArc
        return Shape.Circle arc
    | "Named" ->
        let! name = Decode.item 0 Decode.string
        return Shape.Named name
    | _ -> return! Decoder.errorDecode "Invalid tag"
}

let decode =
    Decode.list decodeShape

[<Tests>]
let tests = test "Sample1" {
    let input = input
//    printfn "%A" (Parsing.parse input)
    let actual = Decode.fromString input decode
    let expected = Ok expected
    Expect.equal actual expected ""
}


