namespace FSharp.Data.Ron

(*
A
A()
A(b)
A(b: c)
()
(b)
(b: c)
*)

type [<RequireQualifiedAccess>]
    AnyStruct =
    | Tag of tag: string
    | Unit of tag: string option
    | Named of tag: string option * content: (string * RonValue) list
    | Unnamed of tag: string option * content: RonValue list

and [<RequireQualifiedAccess>]
    RonValue =
    | AnyStruct of AnyStruct
    
    | List of RonValue list
    | Map of Map<RonValue, RonValue>
    // Primitives
    | Boolean of bool
    | Float of float
    | Integer of int
    | String of string
    | Char of char

module AnyStruct =
    
    // () | T | T()
    let (|StructUnit|_|) = function
        | AnyStruct.Unit tag -> Some tag
        | AnyStruct.Tag tag -> Some (Some tag)
        | _ -> None
    
    // T(v) | (v)
    let (|StructTuple|_|) = function
        | StructUnit tag -> Some (tag, [])
        | AnyStruct.Unnamed (tag, content) -> Some (tag, content)
        | _ -> None
    
    // T(n: v) | (n: v)
    let (|StructNamed|_|) = function
        | StructUnit tag -> Some (tag, [])
        | AnyStruct.Named (tag, content) -> Some (tag, content)
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
    
//    // T | () | T()
//    let (|Unit|_|) = function
//        | StructUnit _ -> Some ()
//        | _ -> None
