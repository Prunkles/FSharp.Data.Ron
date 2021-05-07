namespace FSharp.Data.Ron


[<RequireQualifiedAccess>]
type RonNumber =
    | Signed of int64
    | Unsigned of uint64
    | Float of float

type [<RequireQualifiedAccess>]
    RonStruct =
    /// ()
    | Unit
    /// T() | T
    | Tagged of tag: string * hasBraces: bool
    /// T(n: v) | (n: v)
    | Named of tag: string option * content: (string * RonValue) list
    /// T(v) | (v)
    | Unnamed of tag: string option * content: RonValue list

and [<RequireQualifiedAccess>]
    RonValue =
    | AnyStruct of RonStruct
    | List of RonValue list
    | Map of Map<RonValue, RonValue>
    // Primitives
    | Boolean of bool
    | Number of RonNumber
    | String of string
    | Char of char

module RonStruct =
    
    // () | T | T()
    let (|StructUnit|_|) = function
        | RonStruct.Unit -> Some None
        | RonStruct.Tagged (tag, _) -> Some (Some tag)
        | _ -> None
    
    // T(v) | (v)
    let (|StructTuple|_|) = function
        | StructUnit tag -> Some (tag, [])
        | RonStruct.Unnamed (tag, content) -> Some (tag, content)
        | _ -> None
    
    // T(n: v) | (n: v)
    let (|StructNamed|_|) = function
        | StructUnit tag -> Some (tag, [])
        | RonStruct.Named (tag, content) -> Some (tag, content)
        | _ -> None
    
    // T | T()
    let (|EnumUnit|_|) = function
        | StructUnit (Some tag) -> Some tag
        | _ -> None
    
    // T(v)
    let (|EnumTuple|_|) = function
        | StructTuple (Some tag, content) -> Some (tag, content)
        | _ -> None
    
    // T(n: v)
    let (|EnumNamed|_|) = function
        | StructNamed (Some tag, content) -> Some (tag, content)
        | _ -> None
    
    // (v)
    let (|Tuple|_|) = function
        | StructTuple (None, content) -> Some content
        | _ -> None
    
    // T*
    let (|GetTag|_|) = function
        | EnumUnit name | EnumTuple (name, _) | EnumNamed (name, _) -> Some name
        | _ -> None
    
//    // T | () | T()
//    let (|Unit|_|) = function
//        | StructUnit _ -> Some ()
//        | _ -> None
