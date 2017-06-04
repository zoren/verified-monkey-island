namespace GasdospelaToSMV

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Main =
    open GasdospelaToSMV.AST
    open GasdospelaToSMV.GasdospelaParser
    [<EntryPoint>]
    let main argv =
        //let action = "do", ["it"]
        //let story = [Rule <| ARule(action, ["player_loc", "Inn"], None)]
        let path = "/Users/zoren/Code/verified-monkey-island/webgasdospela/";
        let story = parseFile <| path + "MI2part1.core"
        let all = AbstractInterpreter.findAllStatePairs story
        let s = Graphviz.dotToString <| AstateToGraphviz.statesToGraph all
        printfn "%s" s
        System.IO.File.WriteAllText(path + "mi2.dot", s)
//        printfn "%A" <| AbstractInterpreter.buildAllAStatesForStory story

        // let sw = System.Diagnostics.Stopwatch.StartNew()
        // printfn "%A" <| Generator.runCompact64 story
        // let e = sw.ElapsedMilliseconds
        //printfn "%ims" e
        //let smvString = ConvertSMV.smvFileToString <| ConvertSMV.gasdospelaToSMVDisjunctEndState story

//        let smvString = ConvertSMV.smvFileToString <| ConvertSMV.gasdospelaToSMVFromStory story
        //System.IO.File.WriteAllText("MI2Part1.smv", smvString)
        //printfn "%A" smvString
        0 // return an integer exit code
