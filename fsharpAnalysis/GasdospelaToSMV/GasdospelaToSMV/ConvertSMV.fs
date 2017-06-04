namespace GasdospelaToSMV

module ConvertSMV =
    open GasdospelaToSMV.AST
    open GasdospelaToSMV.Util
       
    let convertRulesImpure rules =
        let m = ref Map.empty
        let rec loop conds =
            function
            | ARule(action, updates, _) ->
                Seq.iter (fun(id, value) -> m := addOneToMany (id, conds) value !m) updates
            | PRule(gasRuleConds, rules) ->
                let ruleConds = gasRuleConds |> List.map convertCond
                Seq.iter (loop (ruleConds @ conds)) rules
        Seq.iter (loop []) rules
        !m

    type Variable = string
    type Value = string    

    type VarDecl = Variable * Value list
    type VarInit = Variable * Value
    type Condition = Variable * ComparisonOperator * Value
    type VarUpdate = Variable * ((Condition list * Value list) list)
    type Transition = Condition list * VarInit list list
    
    type SMVFile = File of VarDecl list * VarInit list * VarUpdate list * Transition list

    let opToString =
        function
        | ComparisonOperator.EQ -> "="
        | ComparisonOperator.NEQ -> "!="

    let condsToString (conds: Condition list) =
        if Seq.isEmpty conds
        then failwith "there should be at least one condition"
        conds |> Seq.map (fun (var, op, value) -> sprintf "%s %s %s" var (opToString op) value) |> String.concat " & "

    let smvFileToString (File(decls, inits, updates, transitions)) =
        let b = System.Text.StringBuilder()
        let appLine s = b.AppendLine s |> ignore
        let appLineIndent n s = b.AppendLine (String.replicate (n * 4) " " + s) |> ignore
        appLine "MODULE main"
        appLine"VAR"
        decls |> Seq.iter (appLineIndent 1 << (fun (var, values) -> sprintf "%s: {%s};" var <| String.concat ", " values))
        appLine "ASSIGN"
        inits |> Seq.iter (appLineIndent 1 << (fun (var, value) -> sprintf "init(%s) := %s;" var value ))
        appLine "-- "
        
        updates
            |> Seq.iter (fun (var, cvls) ->
                            appLineIndent 1 <| sprintf "next(%s) :=" var
                            appLineIndent 2 "case"
                            cvls |> Seq.iter (appLineIndent 3 << (fun (conds, values) -> sprintf "%s: {%s};" (condsToString conds) (String.concat ", " values)))
                            appLineIndent 3 <| sprintf "TRUE: %s;" var
                            appLineIndent 2 "esac;"                            
                        )

        if not <| Seq.isEmpty transitions
        then
            appLineIndent 0 "TRANS"
            appLineIndent 1 "case"
            transitions
                |> Seq.iter
                    (fun(conds, assignmentDisjuncts) ->
                        appLineIndent 1 << sprintf "%s :" <| condsToString conds
                        appLineIndent 2
                            << sprintf "%s;"
                            << String.concat " | "
                            <| Seq.map (String.concat " & " << Seq.map (fun(var, value) -> sprintf "next(%s) = %s" var value)) assignmentDisjuncts

                    )
            appLineIndent 1 "esac;"

        appLine "SPEC"
        appLineIndent 1 "AF EF (money = Money400)"
        b.ToString()

    let actionToSMVValue ((name, args): Action): string = name + String.concat "_" args

    let gasdospelaToSMVNew (story: AST.Decl<_, _> list) =
        let init = getInitblocks story
        let varInit = List.concat init
        let initedVars = varInit |> Seq.map fst |> Set.ofSeq
        let rules = getRules story    
        let flattend = flattenRules rules
        let varAssignmentsRef = ref Map.empty
        let addAssignedValues (updates: Update<'Var, 'Val> list) =
            updates |> Seq.iter (fun (var, value) -> varAssignmentsRef := addOneToManySet var value !varAssignmentsRef)
        addAssignedValues varInit
        flattend |> Seq.iter (fun(_,_,updates,_) -> addAssignedValues updates)
        let actions = flattend |> Seq.map (fun(_,action, _, _) -> actionToSMVValue action) |> set
        let varDeclsMapUndef = Map.map (fun _ v -> Set.add "Undefined" v) !varAssignmentsRef
        let varDeclsMap = Map.add "action" (Set.add "NA" actions) varDeclsMapUndef
        let varDecls = varDeclsMap |> Map.toList |> List.map(fun (var, values) -> var, values |> Set.toList)
        let undefinedVars =
            varDecls
                |> List.filter(fun (var, _) -> not <| Set.contains var initedVars)
                |> List.map (fun (var, _) -> var, "Undefined")
        let ruleToTrans (conds, action: Action, updates : Update<'Var, 'Val> list, _: SideEffect) : Transition =
            let cconds = List.map convertCond conds            
            ("action", EQ, actionToSMVValue action) :: cconds, [("action", "NA") :: updates]
        let trans = flattend |> Seq.map ruleToTrans |> Seq.toList
        File(varDecls, varInit @ undefinedVars, [], trans)

    let gasdospelaToSMVDisjunctEndState (story: AST.Decl<_, _> list) =
        let init = getInitblocks story
        let varInit = List.concat init
        let initedVars = varInit |> Seq.map fst |> Set.ofSeq
        let rules = getRules story    
        let flattend = flattenRules rules
        let varAssignmentsRef = ref Map.empty
        let addAssignedValues (updates: Update<'Var, 'Val> list) =
            updates |> Seq.iter (fun (var, value) -> varAssignmentsRef := addOneToManySet var value !varAssignmentsRef)
        addAssignedValues varInit
        flattend |> Seq.iter (fun(_, _,updates,_) -> addAssignedValues updates)
        let varDeclsMapUndef = Map.map (fun _ v -> Set.add "Undefined" v) !varAssignmentsRef
        let varDeclsMap = varDeclsMapUndef
        let varDecls = varDeclsMap |> Map.toList |> List.map(fun (var, values) -> var, values |> Set.toList)
        let undefinedVars =
            varDecls
                |> List.filter(fun (var, _) -> not <| Set.contains var initedVars)
                |> List.map (fun (var, _) -> var, "Undefined")
        
        let ruleToTrans m (conds, _: Action, updates : Update<'Var, 'Val> list, _: SideEffect) =
            let cconds = List.map convertCond conds            
            addOneToMany cconds updates m
        let condMap = Seq.fold ruleToTrans Map.empty flattend
        let trans = condMap |> Map.toList
        File(varDecls, varInit @ undefinedVars, [], trans)

    let gasdospelaToSMV (init: Update<'Var, 'Val> list list) (m:Map<(Variable * Condition list),  Value list>) =
        let varDeclsMap = ref Map.empty
        Seq.iter (Seq.iter (fun (var, value) -> varDeclsMap := addOneToMany var value !varDeclsMap)) init
        m |> Map.iter (fun (var, _) values -> values |> List.iter (fun value -> varDeclsMap := addOneToMany var value !varDeclsMap))
        let varDecls = Map.toList <| Map.map (fun _ values -> List.distinct <| "Undefined" :: values) !varDeclsMap
        let varInit = List.concat init
        let initedVars = varInit |> Seq.map fst |> Set.ofSeq
        let undefinedVars =
            varDecls
                |> List.filter(fun (var, _) -> not <| Set.contains var initedVars)
                |> List.map (fun (var, _) -> var, "Undefined")
        //let varUpdates =
            //m
                //|> Map.toList
                //|> List.map(fun ((var, conds), values) -> var, [(conds, values)])

        let varUpdates =
            m
                |> Map.toSeq
                |> Seq.groupBy (fun ((var, _), _) -> var)
                |> Seq.map (fun(var, g) -> var, g |> Seq.map(fun((_, cs), vs) -> cs, vs) |> List.ofSeq)
                |> List.ofSeq
                //|> List.map(fun ((var, conds), values) -> var, [(conds, values)])

        File(varDecls, varInit @ undefinedVars, varUpdates, [])

    let gasdospelaToSMVFromStory (story: AST.Decl<_, _> list) =
        let inits = getInitblocks story
        let rules = getRules story
        let m = convertRulesImpure rules
        gasdospelaToSMV inits m
