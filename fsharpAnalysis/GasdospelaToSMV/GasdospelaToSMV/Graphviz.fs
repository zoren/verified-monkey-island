namespace GasdospelaToSMV

module Graphviz =
    type PropertyValue =
        | String of string
        | Value of string
        | Number of string

    type Node = string * Map<string, PropertyValue>
    type Edge = string * string * Map<string, PropertyValue>

    type DotFile =
        string * (Node list) * (Edge list)

    let valueToString =
        function
        | String s -> sprintf "\"%s\"" s
        | Value v -> v
        | Number n -> string n

    let dotToString ((graphName, nodes, edges): DotFile) =
        let sb = System.Text.StringBuilder()
        let appLine s = ignore <| sb.AppendLine s
        let propsToString props =
            if Map.isEmpty props
            then ""
            else sprintf "[%s]" << String.concat "," << Seq.map (fun(pn, pv) -> sprintf "%s=%s" pn <| valueToString pv) <|  Map.toSeq props
        appLine <| sprintf "digraph %s {" graphName
        nodes |> Seq.iter (fun(name, props) -> appLine << sprintf """%s%s;""" name <| propsToString props)
        edges |> Seq.iter (fun(fromNode, toNode, props) -> appLine << sprintf """%s -> %s%s;""" fromNode toNode <| propsToString props)
        appLine "}"
        sb.ToString()

module AstateToGraphviz =
    open AST
    open AbstractInterpreter
    open Graphviz

    let opToString =
        function
        | EQ -> "="
        | NEQ -> "!="

    let valueStateToLabel (id, vs) =
        match vs with
        | Eq s -> sprintf "%s = %s" id s
        | Neq s -> String.concat " & " <| Seq.map (sprintf "%s != %s" id) s

    let astateToLabel (astate:AState) =
        astate |> Map.toSeq |> Seq.map valueStateToLabel |> String.concat " & "

    let opToName =
        function
        | EQ -> "EQ"
        | NEQ -> "NEQ"

    let idToName s =
        let ctoc =
            function
            | '.' -> '_'
            | c -> c
        String.map ctoc s

    let actionToLabel (actionName, args) = sprintf "%s(%s)" actionName <| String.concat ", " args

    let valueStateToName (id, vs) =
        match vs with
        | Eq s -> sprintf "%s_EQ_%s" (idToName id) s
        | Neq s -> String.concat "_" <| Seq.map (sprintf "%s_NEQ_%s" <| idToName id) s

    let astateToName (astate:AState) =
        astate |> Map.toSeq |> Seq.map valueStateToName |> String.concat "_"

    let statesToGraph (nodes: AState seq, edges: (Action * (AState * AState)) seq) : DotFile =
        let implications = AbstractInterpreter.findAllImplications nodes |> Seq.map (fun (f, t) -> astateToName f, astateToName t, Map.ofList ["color", Value "red"])
        let gedges =
            Seq.append
                (Seq.map (fun(a, (f, t)) -> astateToName f, astateToName t, Map.ofList["label", String <| actionToLabel a]) edges)
                (implications)
                |> Seq.toList
        "G", nodes |> Seq.map (fun astate -> astateToName astate, Map.ofList ["label", String <| astateToLabel astate]) |> Seq.toList, gedges
