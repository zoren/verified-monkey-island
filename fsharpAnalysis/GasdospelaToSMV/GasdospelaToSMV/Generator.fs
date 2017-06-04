namespace GasdospelaToSMV

module Generator =
    open AST
    open Util
    open System.Collections.Generic
    let evalCond (env: IDictionary<_, _>) ((el, op, er):Condition<'Var, 'Val>) =
        let evalExp = 
            function
            | VariableDeref v ->
                match env.TryGetValue v with
                | true, value -> value
                | _ -> "Undefined"
            | Constant c -> c
        let compOp =
            match op with
            | EQ -> (=)
            | NEQ -> (<>)
        compOp (evalExp el) (evalExp er)

    let evalCondGeneric env ((el, op, er):Condition<'Var, 'Val>) =
        let evalExp = 
            function
            | VariableDeref v -> env v
            | Constant c -> c
        let compOp =
            match op with
            | EQ -> (=)
            | NEQ -> (<>)
        compOp (evalExp el) (evalExp er)

    let evalConds env conds = List.forall(evalCond env) conds

    let applyUpdates state (updates:Update<'Var, 'Val> list) =
        List.fold (fun m (var, value) -> Map.add var value m) state updates

    type State = Map<string, string>

    let rec getActiveRules state =
        function
        | ARule(action, updates, sideEffect) -> [action, updates, sideEffect]
        | PRule(conds, rules) ->
            if evalConds state conds
            then List.collect (getActiveRules state) rules
            else []

    let iterActiveRules iterator state rules =
        let rec loop =
            function
            | ARule(action, updates, sideEffect) -> iterator (action, updates, sideEffect)
            | PRule(conds, rules) ->
                if evalConds state conds
                then List.iter loop rules
        List.iter loop rules

    let iterActiveRulesGeneric iterator evalConds rules =
        let rec loop =
            function
            | ARule(action, updates, sideEffect) -> iterator (action, updates, sideEffect)
            | PRule(conds, rules) ->
                if evalConds conds
                then List.iter loop rules
        List.iter loop rules

    let rec flattenRule conds =
        function
        | ARule(action, updates, sideEffect) -> Seq.singleton(conds, action, updates, sideEffect)
        | PRule(ruleConds, rules) -> Seq.collect (flattenRule (ruleConds @ conds)) rules

    let getVarDomains (story: Decl<string, string> list) =
        let init = getInitblocks story
        let varInit = List.concat init
        let rules = getRules story
        let varAssignmentsRef = ref Map.empty
        let addAssignedValues updates =
            updates |> Seq.iter (fun (var, value) -> varAssignmentsRef := addOneToManySet var value !varAssignmentsRef)
        addAssignedValues varInit
        let flattend = flattenRules rules
        flattend |> Seq.iter (fun(_, _,updates,_) -> addAssignedValues updates)
        // add Undefined as value to domain of uninited vars
        let initedVars = varInit |> Seq.map fst |> set
        let allVars = !varAssignmentsRef |> Map.toSeq |> Seq.map(fun (var, _) -> var) |> set
        let uninitedVars = Set.difference allVars initedVars
        Seq.fold (fun m uninit -> addOneToManySet uninit "Undefined" m) !varAssignmentsRef uninitedVars

    let ceilLog2 x = int <| System.Math.Ceiling(System.Math.Log(float x, 2.0))

    type CompactState = uint64

    let valueSetToList values =
        if Set.contains "Undefined" values
        then "Undefined" :: (Set.toList <| Set.remove "Undefined" values)
        else Set.toList values

    let runCompact64 (story: Decl<string, string> list) =
        let varDom = getVarDomains story
        let l = varDom |> Map.toList |> List.map (fun (var, valueSet) -> var, Set.count valueSet, ceilLog2 <| Set.count valueSet)
        let offsetDic = Dictionary<_, _>()
        let folder currOffset (var, _, b) =
            let newOffset = currOffset + b
            offsetDic.Add(var, (b, newOffset))
            newOffset
        let bitSum = Seq.fold folder 0 l
        if bitSum >= 64
        then failwith "state to large"
        let getOffset var = offsetDic.[var] |> snd
        let getSize var = offsetDic.[var] |> fst
        let varNameToIndexDict = varDom |> Map.toSeq |> Seq.map fst |> Seq.mapi (fun i v -> v, i) |> dict
        let varToIndex varName = varNameToIndexDict.[varName]
        let valueNameToIndexDict = varDom |> Map.toSeq |> Seq.map (fun (var, values) -> var, values |> valueSetToList |> Seq.mapi (fun i v -> v, i) |> dict) |> dict        
        let valueToIndex varName valueName = uint64 <| valueNameToIndexDict.[varName].[valueName]
        let set var value state = Bit64.setSafe (getOffset var) (getSize var) (valueToIndex var value) state
        let compactState state =
            state |> Map.toSeq |> Seq.fold (fun acc (var, value) -> set var value acc) 0UL
        let applyUpdates (state: CompactState) (updates:Update<_, _> list) =
            List.fold (fun s (var, value) -> set var value s) state updates
        let init = getInitblocks story
        let varInit = List.concat init
        let rules = getRules story
        let compileCond =
            function
            | VariableDeref v, op, Constant c -> fun state -> Bit64.getSafe (getOffset v) (getSize v) state = (valueToIndex v c)
            | _ -> failwith "not supported"
        let compileConds conds =
            let cs = List.map compileCond conds
            fun state -> List.forall (fun pred -> pred state) cs
        let rec compileRule =
            function
            | ARule(action, updates, sideEffect) -> fun _ -> Seq.singleton(action, updates, sideEffect)
            | PRule(ruleConds, rules) ->
                let cs = compileConds ruleConds
                let crs = List.map compileRule rules
                fun state ->
                    if cs state
                    then Seq.collect (fun f -> f state) crs
                    else Seq.empty
        let compileRules rules =
            let crs = List.map compileRule rules
            fun state -> Seq.collect (fun f -> f state) crs
        let visitedStates = HashSet<CompactState>()
        let initState = varInit |> Map.ofList
        let stack = new Stack<CompactState>()
        stack.Push <| compactState initState
        let compiledRules = compileRules rules
        while visitedStates.Count < 50000 && stack.Count <> 0 do
            let state = stack.Pop()
            printfn "state %s" <| Bit64.toString state
            ignore <| visitedStates.Add state
            let activeRules = compiledRules state
            printfn "active: %i" <| Seq.length activeRules
            activeRules |>
                Seq.iter
                    (fun (_, updates, _) ->
                                let newState = applyUpdates state updates
                                if not <| visitedStates.Contains newState
                                then stack.Push newState)
        stack.Count        
    
    let run (story: Decl<string, string> list) =
        let init = getInitblocks story
        let varInit = List.concat init
        let rules = getRules story
        let visitedStates = HashSet<Map<string, string>>()
        let initState = varInit |> Map.ofList
        let stack = new Stack<_>()
        stack.Push initState
        while visitedStates.Count < 50000 && stack.Count <> 0 do
            let state = stack.Pop()
            ignore <| visitedStates.Add state
            iterActiveRules
                (fun (_, updates, _) ->
                            let newState = applyUpdates state updates
                            if not <| visitedStates.Contains newState
                            then stack.Push newState)
                state rules
        stack.Count
    
