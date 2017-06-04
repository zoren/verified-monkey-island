namespace GasdospelaToSMV

module Util =
    open GasdospelaToSMV.AST
    let getInitblocks (s: Decl<_, _> list) =
        s |> List.choose (function InitBlock b -> Some b | _ -> None)

    let getRules (s: Decl<_, _> list) =
        s |> List.choose (function Rule b -> Some b | _ -> None)

    let addOneToMany k v m =
        let l = defaultArg (Map.tryFind k m) []
        Map.add k (v::l) m

    let addOneToManySet k v m =
        let s = defaultArg (Map.tryFind k m) Set.empty
        Map.add k (Set.add v s) m

    let convertCond (cond: AST.Condition<_, _>) =
        match cond with
        | VariableDeref var, op, Constant c -> var, op, c
        | _ -> failwith "expected variable = value"

    let rec flattenRule conds =
        function
        | ARule(action, updates, sideEffect) -> Seq.singleton(conds, action, updates, sideEffect)
        | PRule(ruleConds, rules) -> Seq.collect (flattenRule (ruleConds @ conds)) rules
    
    let flattenRules rules = Seq.collect (flattenRule []) rules
