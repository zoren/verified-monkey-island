import * as lang from "./lang";

function concatMany<T>(arrays: T[][]): T[] {
    let res: T[] = [];
    arrays.forEach((ar) => res = res.concat(ar));
    return res;
}

export type Store = (string) => string | undefined

function evalComp(compOp: lang.ComparisonOperator){
    switch(compOp){
        case lang.ComparisonOperator.EQ:
            return (l, r) => l === r;
        case lang.ComparisonOperator.NEQ:
            return (l, r) => l !== r;
    }
}

export function evalConds(get: Store, conds: lang.Condition[]){
    function evalCond(cond: lang.Condition) {
        let evalExp = (e: lang.Expression) => e instanceof lang.Constant ? e.value : get(e.name);
        let vl = evalExp(cond.expl);
        let vr = evalExp(cond.expr);
        let f = evalComp(cond.compOperator);
        return f(vl, vr);
    }
    return conds.every(evalCond);
}

export function getAvailableActionRules(get: Store, rules: lang.Rule[]): lang.ARule[] {
    function getAvailableActions(rule: lang.Rule): lang.ARule[] {
        if (rule instanceof lang.ARule) {
            return [rule];
        } else
            if (rule instanceof lang.PRule) {
                if (evalConds(get, rule.preconditions)) {
                    return concatMany(rule.rules.map(getAvailableActions));
                }
                return [];
            }
        throw new Error();
    }
    return concatMany(rules.map(getAvailableActions));
}

export function getInitialStateDecls(story: lang.Story) {
    let initBlocks: lang.Update[][] = []
    story.decls.forEach((decl) => {if(decl instanceof lang.InitBlock) {initBlocks.push(decl.updates)}});
    return initBlocks;
}

export function getRulesDecls(story: lang.Story) {
    let rules: lang.Rule[] = []
    story.decls.forEach((decl) => {
        if (decl instanceof lang.ARule || decl instanceof lang.PRule) { rules.push(decl) }
    });
    return rules;
}
