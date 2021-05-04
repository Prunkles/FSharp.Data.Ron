[<AutoOpen>]
module internal FSharp_Data_Ron_Utils

let (|Equals|_|) x y = if x = y then Some () else None
