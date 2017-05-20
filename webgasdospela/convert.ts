import { NamedBuilder, NDDBuilder } from "nmdd-builder";
import * as lang from "./lang"
import * as infer from "./infer"
import * as interpreter from "./interpreter"

let mkBeforeName = (s: string) => "before." + s;
let mkAfterName = (s: string) => "after." + s;

export function calcReacableStates(story: lang.Story) {
    let doms = infer.getStoryDomains(story);
    let vars = new Map<string, Set<string>>()
    let order: string[] = [];
    let before: string[] = [];
    let after: string[] = [];

    for(let k of doms.updatesMap.keys()){
        let b = mkBeforeName(k);
        let a = mkAfterName(k);
        let domSet = new Set(doms.updatesMap.get(k));
        order.push(b);
        order.push(a);
        vars.set(b, domSet);
        vars.set(a, domSet);
        before.push(b);
        after.push(a);
    }
    let b = new NamedBuilder(vars, order);
    
    function convertCondition(condition: lang.Condition) {
        if(condition.compOperator === lang.ComparisonOperator.EQ) {
            if(condition.expl instanceof lang.VariableDeref && condition.expr instanceof lang.Constant){
                return b.MakeEq(condition.expl.name, condition.expr.value);
            }
            if(condition.expl instanceof lang.Constant && condition.expr instanceof lang.VariableDeref){
                return b.MakeEq(condition.expl.value, condition.expr.name);
            }
        }
        throw new Error("not implemented");
    }

    function convertUpdates(u: lang.InitBlock){
        return u.updates.reduce((acc, u) => b.ApplyBoolean(NDDBuilder.And, [acc, b.MakeEq(mkBeforeName(u.name), u.constant.value)]), 1);
    }

    function convertRule(rule: lang.Rule) {
        if(rule instanceof lang.ARule){
            let updateNdds = rule.updates.reduce((acc, u) => b.ApplyBoolean(NDDBuilder.And, [acc, b.MakeEq(mkAfterName(u.name), u.constant.value)]), 1);
            let updatedVars = new Set<string>(rule.updates.map(u => u.name));
            let res = updateNdds;
            vars.forEach((_, variable) => {
                if(!updatedVars.has(variable)){
                    res = b.ApplyBoolean(NDDBuilder.And, [res, b.MakeVariableEq(mkBeforeName(variable), mkAfterName(variable))]);
                }
            });
            return res;
        }
        if(rule instanceof lang.PRule){
            let cs = rule.preconditions.map(convertCondition);
            let pre = cs.reduce((x, y) => b.ApplyBoolean(NDDBuilder.And, [x, y]))
            return rule.rules
                    .map(r => b.ApplyBoolean(NDDBuilder.And, [pre, convertRule(r)]))
                    .reduce((x, y) => b.ApplyBoolean(NDDBuilder.Or, [x, y]))
        }
    }
    
    function convertRules(rules: lang.Rule[]) {
        return rules.reduce((acc, r) => b.ApplyBoolean(NDDBuilder.Or, [acc, convertRule(r)]), 0)
    }

    function ReachableStates(I: number, T: number, x: string[], xp: string[]) {
        let varMap = new Map(x.map((xv, i) => [xv, xp[i]] as [string, string]));
        let R = 0;
        let Rp: number;
        do {
            Rp = R;
            let tr = b.ApplyBoolean(NDDBuilder.And, [T, R]);
            let Etr = x.reduce((acc, xv) => b.Exists(xv, acc), tr);
            let IEtr = b.ApplyBoolean(NDDBuilder.Or, [I, Etr]);
            R = b.CompositionNVar(IEtr, varMap);
        } while (Rp !== R);
        return R;
    }

    let initialDecls = interpreter.getInitialStateDecls(story);
    let initialState = 1;
    initialDecls.forEach(updates =>
        updates.forEach(update => initialState = b.ApplyBoolean(NDDBuilder.And, [initialState, b.MakeEq(update.name, update.constant.value)])));

    let rules = interpreter.getRulesDecls(story);
    let T = rules.map(convertRule).reduce((acc, r) => b.ApplyBoolean(NDDBuilder.Or, [acc, r]), 0);
    return ReachableStates(initialState, T, before, after);
}
