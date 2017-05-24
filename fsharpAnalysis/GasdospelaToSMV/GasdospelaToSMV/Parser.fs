namespace GasdospelaToSMV

open FParsec
open AST

module GasdospelaParser =

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

    let arule: Parser<Rule> =
        tuple3
            (action .>> gtgt)
            (inBraces updates)
            (opt(amp >>. inBraces(qt >>. manySatisfy ((<>)'\"') .>> qt)))
            |>> ARule

    let comparisons = sepBy(tuple3 exp compOp exp) comma

    let rule, ruleRef = createParserForwardedToRef<Rule, unit>()

    let getColumn = getPosition |>> (fun pos -> pos.Column)

    let TabSize = 4L

    let atNextTabStop = getPosition >>= (fun pos -> userStateSatisfies (fun curr -> curr = (pos.Column - TabSize)))

    let prule: Parser<Rule> =
        getColumn .>>. inBraces comparisons .>> question
            >>= (fun(startColumn, comps) ->
                    many1(getColumn >>= (fun col -> if col = startColumn + TabSize then rule else pzero))
                        |>> (fun rules -> PRule(comps, rules)))

    do ruleRef := prule <|> arule

    let initialDecl = initial >>. inBraces updates |>> InitBlock

    let decl = attempt (initialDecl <|> (rule |>> Rule))
    let decls = (spaces >>. many decl .>> eof)

    let parseString str = run decls str
    let parseFile path =
        match runParserOnFile decls () path System.Text.Encoding.UTF8 with
        | Success(s,_,_) -> s
        | Failure(_, f, _) -> failwithf "error: %A" f
