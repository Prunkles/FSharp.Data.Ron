module FSharp.Data.Ron.Printing

open System.Text

let print (value: RonValue) : string =
    let sb = StringBuilder()
    let printIndent indent =
        let tab = "    "
        sb.Append(String.replicate indent tab) |> ignore
    let printf fmt = Printf.kprintf (sb.Append >> ignore) fmt
    let printfn indent fmt =
        Printf.kprintf (fun s -> sb.AppendLine(s) |> ignore; printIndent indent) fmt
    
    let rec printValue indent value =
        match value with
        | RonValue.Char c -> printf $"'%c{c}'"
        | RonValue.Boolean b -> printf $"%b{b}"
        | RonValue.String s -> printf $"\"%s{s}\""
        | RonValue.Integer i -> printf $"%i{i}"
        | RonValue.Float f -> printf $"%f{f}"
        
        | RonValue.List vs ->
            printf "["
            for v in vs do
                printfn (indent + 1) ""
                printValue (indent + 1) v
                printf ","
            printfn indent ""
            printf "]"
        | RonValue.Map m ->
            printf "{"
            for KeyValue(k, v) in m do
                printfn (indent + 1) ""
                printValue (indent + 1) k
                printf ": "
                printValue (indent + 1) v
                printf ","
            printfn indent ""
            printf "}"
        
        | RonValue.AnyStruct s ->
            let sprintTag = function Some t -> t | None -> ""
            match s with
            | AnyStruct.Tag tag -> printf $"%s{tag}"
            | AnyStruct.Unit tag -> printf $"{sprintTag tag}()"
            | AnyStruct.Unnamed (tag, content) ->
                printf $"{sprintTag tag}"
                printf "("
                for v in content do
                    printfn (indent + 1) ""
                    printValue (indent + 1) v
                    printf ","
                printfn indent ""
                printf ")"
            | AnyStruct.Named (tag, content) ->
                printf $"{sprintTag tag}"
                printf "("
                for n, v in content do
                    printfn (indent + 1) ""
                    printf $"{n}: "
                    printValue (indent + 1) v
                    printf ","
                printfn indent ""
                printf ")"
    
    printValue 0 value
    
    sb.ToString()