namespace GasdospelaToSMV

module AbstractInterpreter =
    open AST

    type VarState =
        | Eq of string
        | Neq of Set<string>

    type AState = Map<string, VarState>

    let getVarOpConst =
        function
        | VariableDeref v, op, Constant c -> v, op, c
        | _ -> failwith "not supported"

    let buildAstate (astate:AState) cond =
        let v, op, c = getVarOpConst cond
        match op with
        | EQ ->
            if Map.containsKey v astate
            then failwith "variable already mapped"
            Map.add v (Eq c) astate
        | NEQ ->
            let s =
                match Map.tryFind v astate with
                | Some(Neq s) -> s
                | Some(Eq _) -> failwith "var already mapped"
                | None -> Set.empty
            Map.add v (Neq (Set.add c s)) astate

    let applyUpdate (astate:AState) (var, value) = Map.add var (Eq value) astate

    let rec buildAStatesForRule astate =
        function
        | ARule(_, updates, _) -> Seq.singleton <| Seq.fold applyUpdate astate updates
        | PRule(conds, rules) ->
            let v = (Seq.fold buildAstate astate conds)
            Seq.collect (buildAStatesForRule v) rules        

    let evalCond (astate:AState) cond =
        let v, op, c = getVarOpConst cond
        match Map.tryFind v astate with
        | Some(Eq fc) -> Some(c = fc)
        | Some(Neq fs) ->
            match op with
            | EQ -> Some (not <| Set.contains c fs)
            | NEQ -> Some (Set.contains c fs)
        | None -> None

    let andTri bt1 bt2 =
        match bt1, bt2 with
        | Some b1, Some b2 -> Some (b1 && b2)
        | _ -> None

    let rec evalRule (astate:AState) =
        function
        | ARule(_, updates, _) -> Seq.singleton <| Seq.fold applyUpdate astate updates
        | PRule(conds, rules) ->
            if Seq.map (evalCond astate) conds |> Seq.reduce andTri = Some true
            then Seq.collect (evalRule astate) rules
            else Seq.empty

    let buildAStatesForStory story =
        let rules = Util.getRules story
        Seq.collect (buildAStatesForRule Map.empty) rules

    let impliesVar (op, v) (fop, fv) =
        match op, fop with
        | EQ, EQ -> Some (v = fv)
        | EQ, NEQ -> Some (v <> fv)
        | NEQ, EQ ->
            if v = fv
            then Some false
            else None
        | NEQ, NEQ -> 
            if v = fv
            then Some true
            else None

    let impliesVars vs1 vs2 =
        match vs1, vs2 with
        | Eq v, Eq fv -> Some (v = fv)
        | Eq v, Neq fs -> Some (not <| Set.contains v fs)
        | Neq s, Eq fv ->
            if s = Set.singleton fv
            then Some false
            else None
        | Neq s, Neq fs -> 
            if Set.isSubset s fs
            then Some true
            else None

    let impliesVarState (from: AState) (x, vs) =
        match Map.tryFind x from with
        | Some fvs -> impliesVars fvs vs
        | None -> None

    let implies  (from: AState) (toState: AState) =
        toState |> Map.toSeq |> Seq.map (impliesVarState from) |> Seq.reduce andTri

    open System.Collections.Generic

    let findAllAStates rules givenAstate =
        let visitedStates = HashSet<AState>()
        let edges = List<AState * AState>()
        let stack = new Stack<AState>()
        stack.Push givenAstate
        while stack.Count <> 0 do
            let state = stack.Pop()
            ignore <| visitedStates.Add state
            Seq.collect (evalRule state) rules
                |> Seq.filter(not << visitedStates.Contains)
                |> Seq.iter (fun s ->
                                edges.Add((state, s))
                                stack.Push s)
        visitedStates, edges

    let findAllImplications astates =
        astates |> Seq.collect (fun astate -> astates |> Seq.filter (fun astate1 -> astate <> astate1 && implies astate astate1 = Some true) |> Seq.map (fun a -> astate, a))

    let findAllImplicationsStory story =
        let rules = Util.getRules story
        let astates = Seq.collect (buildAStatesForRule Map.empty) rules
        let implications = findAllImplications astates
        set astates, set implications

    let buildAllAStatesForStory story =
        let rules = Util.getRules story
        let astates = Seq.collect (buildAStatesForRule Map.empty) rules
        astates |> Seq.map (fun astate -> astate, findAllAStates rules astate) |> Seq.toList

    let rec buildAStatePairsForRule astate =
        function
        | ARule(action, updates, _) -> Seq.singleton (action, (astate, Seq.fold applyUpdate astate updates))
        | PRule(conds, rules) ->
            let v = Seq.fold buildAstate astate conds
            Seq.collect (buildAStatePairsForRule v) rules  

    let findAllStatePairs story =
        let rules = Util.getRules story
        let astatePairs = Seq.collect (buildAStatePairsForRule Map.empty) rules |> set   
        let states = Set.map snd astatePairs
        Set.union (Set.map fst states) (Set.map snd states), astatePairs
