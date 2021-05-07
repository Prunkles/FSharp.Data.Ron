module FSharp.Data.Ron.Parsing

open System
open FParsec
open FSharp.Data.Ron

// https://github.com/ron-rs/ron/blob/995d9e93d98b93cfbf0d6040230f6732dd4cf32b/docs/grammar.md
module Grammar =

    open System.Text
    
    let mapUserState (mapping: 'v -> 'u) (p: Parser<_, 'u>) : Parser<_, 'v> =
        fun streamV ->
            let userStateV = streamV.UserState
            let userStateU = mapping userStateV
            let streamU = streamV.CreateSubstream<'u>(streamV.State)
            streamU.UserState <- userStateU
            p streamU
    
    let valueR, valueRef = createParserForwardedToRef<RonValue, _> ()
    
    let ident =
        let isAsciiIdStart c = isAsciiLetter c || c = '_'
        let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_'
        identifier (
            IdentifierOptions(
                isAsciiIdStart = isAsciiIdStart,
                isAsciiIdContinue = isAsciiIdContinue
            ))
    
    // ----------------
    // Whitespace and comments
    // ----------------
    
    // ws_single = "\n" | "\t" | "\r" | " ";
    let ws_single = anyOf " \n\t\r"
    
    // comment = ["//", { no_newline }, "\n"] | ["/*", { ? any character ? }, "*/"];
    let comment =
        let singleline = pstring "//" >>. manyChars (noneOf "\n") .>> pchar '\n' <?> "SingleLineComment"
        let multiline =
            let p: Parser<_, _> = fun stream ->
                let sb = StringBuilder()
                if stream.PeekString(2) <> "/*" then
                    Reply(Error, NoErrorMessages)
                else
                    stream.Skip(2)
                    let mutable deep = 1
                    while deep > 0 && (stream.Peek() <> Char.MaxValue) do
                        let next = stream.PeekString(2)
                        match next with
                        | "/*" -> deep <- deep + 1
                        | "*/" -> deep <- deep - 1
                        | _ -> ()
                        sb.Append(stream.Read()) |> ignore
                    
                    if deep > 0 then
                        Reply(FatalError, expected "closing */")
                    else
                        stream.Skip(1)
                        Reply(sb.ToString())
            p <?> "MultilineComment"
        singleline <|> multiline
    
    // ws = { ws_single, comment };
    let ws = skipMany ((ws_single >>% ()) <|> (comment >>% ())) <?> "whitespace"
    
    // ----------------
    // Commas
    // ----------------
    
    // comma = ws, ",", ws;
    let comma = ws >>? pchar ',' .>> ws
    
    // ----------------
    // Extensions
    // ----------------
    
    let extensions_name =
        // choice [ pstring "unwrap_newtypes"; pstring "implicit_some" ]
        ident
    
    // extensions_inner = "enable", ws, "(", extension_name, { comma, extension_name }, [comma], ws, ")";
    let extensions_inner =
        pstring "enable" >>. ws >>. pchar '(' >>. (sepEndBy1 (extensions_name .>> ws) comma) .>> ws .>> pchar ')'
    
    // extensions = { "#", ws, "!", ws, "[", ws, extensions_inner, ws, "]", ws };
    let extensions =
        let extension =
            pchar '#' >>. ws
            >>. pchar '!' >>. ws
            >>. pchar '[' >>. ws
            >>. extensions_inner .>> ws
            .>> pchar ']'
        many (extension .>> ws)
        |>> List.concat
    
    // ----------------
    // Numbers
    // ----------------
    
    type NumberType =
        | Unsigned = 0
        | Signed = 1
        | Float = 2
    
    [<Struct>]
    type ParsedNumber =
        { Data: string
          Type: NumberType }
    
    let numberP: Parser<ParsedNumber,_> =
        fun stream -> 
            // Store state
            let index0 = stream.IndexToken
            let stateTag0 = stream.StateTag
            // inner state
            let mutable c = stream.Peek()
            let mutable isSigned = false
            
            // # helpers
            let inline isBinary (ch: char) = ch = '0' || ch = '1'
            let inline isSign (ch: char) = ch = '+' || ch = '-'
            let inline isE (ch: char) = ch = 'e' || ch = 'E'
            
            // return true if skip 1 or more
            let inline skipAndPeekWhile1 (cond: char -> bool) =
                let skip1 = cond c
                if skip1 then
                    c <- stream.SkipAndPeek()
                    while cond c do c <- stream.SkipAndPeek()
                    true
                else
                    false
            
            let inline getExpectedIntegerType () =
                if isSigned then NumberType.Signed else NumberType.Unsigned 
            
            let inline replyParsedNumber (ty: NumberType) =
                let r = stream.ReadFrom(index0)
                Reply({ParsedNumber.Data = r; ParsedNumber.Type = ty})
            
            let inline replyError err =
                // Restore state
                stream.Seek(index0)
                stream.StateTag <- stateTag0
                Reply(Error, err)
            
            let inline replyFatalError err =
                Reply(Error, err)
            
            // # impl
            
            // parse after 'e' : '0.3e{}' '.3e{}'
            let inline parseExpPart () =
                if isSign c then
                    c <- stream.SkipAndPeek()
                if skipAndPeekWhile1 isDigit then
                    replyParsedNumber NumberType.Float
                else 
                    replyFatalError (expected "exponent power")
            
            // parse after '.' : '0.{not req digits}[e{digits}]' '.{req digits}[e{digits}]'
            let inline parseFractionalPart (reqFractionalDigits: bool) =
                let hasDigits = skipAndPeekWhile1 isDigit
                if (not hasDigits) && reqFractionalDigits then
                    replyFatalError (expected "fractional digits")
                else
                if isE c then
                    c <- stream.SkipAndPeek()
                    parseExpPart ()
                else
                    replyParsedNumber NumberType.Float
                
            // # parsing
            // check sign first
            if isSign c then
                isSigned <- true
                c <- stream.SkipAndPeek()
            
            let inline parseSpecialInteger checker expectedMsg =
                if skipAndPeekWhile1 checker
                then replyParsedNumber (getExpectedIntegerType ())
                else replyFatalError (expected expectedMsg)
            
            // check 0x | 0b | 0o | i | I | n | N prefixes
            let c0, c1 = c, stream.Peek(1)
            match c0, c1 with
            | '0', 'x' ->
                c <- stream.SkipAndPeek(2)
                parseSpecialInteger isHex "hex digits"
            | '0', 'o' ->
                c <- stream.SkipAndPeek(2)
                parseSpecialInteger isOctal "octal digits"
            | '0', 'b' ->
                c <- stream.SkipAndPeek(2)
                parseSpecialInteger isBinary "binary digits"
            // parse +inf/-inf/+NaN/-NaN
            | 'i', _ | 'I', _ ->
                c <- stream.SkipAndPeek(2) // c2
                let c2 = c
                if c1 = 'n' && c2 = 'f'
                then
                    c <- stream.SkipAndPeek()
                    replyParsedNumber NumberType.Float
                else replyError (expected "inf")
            | 'n', _ | 'N', _ ->
                c <- stream.SkipAndPeek(2) // c2
                let c2 = c
                if c1 = 'a' && (c2 = 'n' || c2 = 'N')
                then
                    c <- stream.SkipAndPeek()
                    replyParsedNumber NumberType.Float
                else replyError (expected "NaN")
            // other variants
            | _ ->
                // other variants
                let hasIntegerPart = skipAndPeekWhile1 isDigit
                match c with 
                | '.' when hasIntegerPart -> 
                    c <- stream.SkipAndPeek()
                    parseFractionalPart false
                | '.' when not hasIntegerPart -> 
                    c <- stream.SkipAndPeek()
                    parseFractionalPart true
                // {N}e{P}
                | ch when (isE ch) && hasIntegerPart ->
                    c <- stream.SkipAndPeek()
                    parseExpPart ()
                | _ when hasIntegerPart ->
                    replyParsedNumber (getExpectedIntegerType ())
                | _ ->
                    // does not start with a digit or a dot or a sign
                    match isSigned with
                    | true -> replyError (expected "Number digit, .")
                    | false -> replyError (expected "Number digit, ., +, -")
                    
    
    let numberR =
        numberP
        |>> fun pNum ->
            match pNum.Type with
            | NumberType.Unsigned ->
                uint64 pNum.Data |> RonNumber.Unsigned 
            | NumberType.Signed ->
                int64 pNum.Data |> RonNumber.Signed
            | NumberType.Float ->
                match pNum.Data with
                // Standard parser 
                | "inf" | "Inf" | "+inf" | "+Inf" ->
                    infinity |> RonNumber.Float
                | "-inf" | "-Inf" -> Double.NegativeInfinity |> RonNumber.Float
                | _ -> float pNum.Data |> RonNumber.Float
            | _ -> ArgumentOutOfRangeException() |> raise
            |> RonValue.Number
    
    // ----------------
    // String
    // ----------------
    
    // string_raw_content = ("#", string_raw_content, "#") | "\"", { unicode_non_greedy }, "\"";
    let string_raw_content: Parser<string, _> =
        fun stream ->
            let mutable hashCount = 0
            while (stream.Peek() = '#') do
                stream.Read() |> ignore
                hashCount <- hashCount + 1
            let expectedHashes = String.replicate hashCount "#"
            if stream.Read() <> '"' then
                Reply(Error, expected "\"")
            else
                let sb = StringBuilder()
                let mutable doLoop = true
                while doLoop do
                    let c = stream.Read()
                    if c = '"' then
                        let hashes = stream.PeekString(hashCount)
                        if hashes = expectedHashes then
                            stream.Skip(hashCount)
                            doLoop <- false
                        else
                            sb.Append("\"") |> ignore
                    else
                        sb.Append(c) |> ignore
                let result = sb.ToString()
                Reply(result)
    
    // string_raw = "r" string_raw_content;
    let string_raw = pchar 'r' >>? string_raw_content
    
    // string_escape = "\\", ("\"" | "\\" | "b" | "f" | "n" | "r" | "t" | ("u", unicode_hex));
    let string_escape =
        let unicode_hex = many1Chars hex
        pchar '\\' .>>. ((anyOf [ '"'; '\\'; 'b'; 'f'; 'n'; 'r'; 't' ] |>> string) <|> ((pchar 'u' |>> string) .>>. unicode_hex |>> (fun (x, y) -> x + y)))
        |>> fun (c, s) -> string c + s
    
    // string_std = "\"", { no_double_quotation_marks | string_escape }, "\"";
    let string_std =
        let no_double_quotation_marks = noneOf "\"" |>> string
        between (pchar '"') (pchar '"')
            (manyStrings (no_double_quotation_marks <|> string_escape))
    
    // string = string_std | string_raw;
    let string' =
        string_std <|> string_raw
    
    let stringR = string' |>> RonValue.String
    
    // ----------------
    // Char
    // ----------------
    
    // char = "'", (no_apostrophe | "\\\\" | "\\'"), "'";
    let char' =
        between (pchar '\'') (pchar '\'')
            ((noneOf ['''] |>> string) <|> pstring @"\\" <|> pstring @"\'" )
        |>> function @"\\" -> '\\' | @"\'" -> ''' | c -> char c
    
    let charR = char' |>> RonValue.Char
    
    // ----------------
    // Boolean
    // ----------------
    
    // bool = "true" | "false";
    let bool' =
        let true' = stringReturn "true" true
        let false' = stringReturn "false" false
        true' <|> false'
    
    let boolR = bool' |>> RonValue.Boolean
    
//    // ----------------
//    // Optional
//    // ----------------
//    
//    // option = "Some", ws, "(", ws, value, ws, ")";
//    let option' = pstring "Some" >>. ws >>. pchar '(' >>. ws >>. valueR .>> ws .>> pchar ')'
    
    // ----------------
    // List
    // ----------------
    
    // list = "[", [value, { comma, value }, [comma]], "]";
    let listP =
        between (pchar '[') (pchar ']')
            (ws >>. sepEndBy (valueR .>> ws) (comma >>. ws))
    
    let listR = listP |>> RonValue.List
    
    // ----------------
    // Map
    // ----------------
    
    // map_entry = value, ws, ":", ws, value;
    let map_entry = valueR .>> ws .>> pchar ':' .>> ws .>>. valueR
    
    // map = "{", [map_entry, { comma, map_entry }, [comma]], "}";
    let mapP =
        between (pchar '{') (pchar '}')
            (ws >>. sepEndBy (map_entry .>> ws) (comma >>. ws))
    
    let mapR = mapP |>> (Map.ofList >> RonValue.Map)
    
    // ----------------
    // AnyStruct
    // ----------------
    
    // named_field = ident, ws, ":", value;
    let named_field = ident .>> ws .>>? pchar ':' .>> ws .>>. valueR
    
    let anyStruct =
        let unitContent    = pchar '(' >>? ws >>? pchar ')' >>% ()
        let namedContent   = pchar '(' >>? (ws >>? sepEndBy1 (named_field .>>? ws) comma) .>> pchar ')'
        let unnamedContent = pchar '(' >>? (ws >>? sepEndBy1 (valueR      .>>? ws) comma) .>> pchar ')'
        
        let unitStruct    = unitContent
        let taggedStruct  =     ident .>>? ws .>>.? (opt unitContent |>> Option.isSome)
        let namedStruct   = opt ident .>>? ws .>>.? namedContent
        let unnamedStruct = opt ident .>>? ws .>>.? unnamedContent
        choice [
            unitStruct    |>> fun () -> RonStruct.Unit
            namedStruct   |>> RonStruct.Named
            unnamedStruct |>> RonStruct.Unnamed
            taggedStruct  |>> RonStruct.Tagged
        ]
    
    let anyStructR = anyStruct |>> RonValue.AnyStruct
    
    // ----------------
    // Value
    // ----------------
    
    // value = unsigned | signed | float | string | char | bool | option | list | map | tuple | struct | enum_variant;
    do valueRef := choice [
        numberR
        stringR
        charR
        boolR
        anyStructR
        listR
        mapR
    ]
    
    // ----------------
    // RON file
    // ----------------
    
    // RON = [extensions], ws, value, ws;
    let RON =
        ws >>. opt extensions .>> ws .>>. valueR .>> ws .>> eof
        |>> function e, v -> (match e with None -> [] | Some es -> es), v


type RonFile =
    { Extensions: string list
      Value: RonValue }

let parse input =
    let result = runParserOnString Grammar.RON () "" input
    match result with
    | Success ((exts, value), _, _) -> Result.Ok { Extensions = exts; Value = value }
    | Failure (errorMsg, _, _) -> Result.Error errorMsg
