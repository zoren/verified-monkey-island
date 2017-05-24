namespace GasdospelaToSMV

module ConvertSMV =
    open GasdospelaToSMV.AST
    let getInitblocks (s: Decl list) =
        s |> List.choose (function InitBlock b -> Some b | _ -> None)

    let getRules (s: Decl list) =
        s |> List.choose (function Rule b -> Some b | _ -> None)

    let addOneToMany k v m =
        let l = defaultArg (Map.tryFind k m) []
        Map.add k (v::l) m

    let convertCond (cond: AST.Condition) =
        match cond with
        | VariableDeref var, op, Constant c -> var, op, c
        | _ -> failwith "expected variable = value"

    //let convertRulesPure rules =
        //let rec convertRuleJA conds m =
        //    function
        //    | ARule(action, updates, _) ->
        //        updates |> Seq.fold(fun s (id, value) -> addOneToMany (id, conds) value s) m
        //    | PRule(gasRuleConds, rules) ->
        //        let ruleConds = gasRuleConds |> List.map convertCond
        //        Seq.fold (convertRuleJA (ruleConds @ conds)) m rules
        //Seq.fold (convertRuleJA []) Map.empty rules

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
    
    type SMVFile = File of VarDecl list * VarInit list * VarUpdate list

    let opToString =
        function
        | ComparisonOperator.EQ -> "="
        | ComparisonOperator.NEQ -> "!="

    let condsToString (conds: Condition list) =
        if conds |> Seq.length = 0 
        then failwith "there should be at least one condition"
        conds |> Seq.map (fun (var, op, value) -> sprintf "%s %s %s" var (opToString op) value) |> String.concat " & "

    let smvFileToString (File(decls, inits, updates)) =
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
        appLine "SPEC"
        appLineIndent 1 "AF EF (money = Money400)"
        b.ToString()

    let gasdospelaToSMV (init: Update list list) (m:Map<(Variable * Condition list),  Value list>) =
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

        File(varDecls, varInit @ undefinedVars, varUpdates)

    let gasdospelaToSMVFromStory (story: AST.Decl list) =
        let inits = getInitblocks story
        let rules = getRules story
        let m = convertRulesImpure rules
        gasdospelaToSMV inits m
