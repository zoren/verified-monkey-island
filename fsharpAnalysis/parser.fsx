#load "ast.fs"
#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"

open FParsec
open AST

type Parser<'Result> = Parser<'Result, unit>

let token (s:string) : Parser<unit> = skipString s  .>> spaces
let setEq = token ":="
let isEq = token "="
let notEq = token "!="
let comma = token ","
let semiColon = token ";"
let lbrace = token "{"
let rbrace = token "}"
let lpar = token "("
let rpar = token ")"
let question = token "?"
let amp = token "&"
let gtgt = token ">>"
let qt = token "\""

let initial = token "Initial"

let isAsciiIdStart c =
    isUpper c || c = '_'

let isAsciiIdContinue c =
    isAsciiLetter c || isDigit c

let ctor = identifier (IdentifierOptions(isAsciiIdStart    = isUpper,
                              isAsciiIdContinue = isAsciiIdContinue)) .>> spaces

let isAsciiIdContinueVar c =
    isAsciiLetter c || isDigit c || c = '.'

let var = identifier (IdentifierOptions(isAsciiIdStart    = isLower,
                              isAsciiIdContinue = isAsciiIdContinueVar)) .>> spaces

let inPars p = lpar >>. p .>> rpar
let inBraces p = lbrace >>. p .>> rbrace

let action = var .>>. (inPars <| sepBy var comma)

let update: Parser<Update> = var .>> setEq .>>. ctor

let updates = sepBy update semiColon

let exp =
    (var |>> VariableDeref)
        <|>
    (ctor |>> Constant)

let compOp =
    (isEq >>% EQ)
        <|>
    (notEq >>% NEQ)

let condition = tuple3 exp compOp exp

let arule =
    tuple3
        (action .>> gtgt)
        (inBraces updates)
        (amp >>. inBraces(qt >>. manySatisfy ((<>)'\"') .>> qt))

let parseString str = run arule str
