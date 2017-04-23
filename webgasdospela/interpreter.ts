import * as lang from "./lang";

function concatMany<T>(arrays: T[][]): T[] {
    let res: T[] = [];
    arrays.forEach((ar) => res = res.concat(ar));
    return res;
}

export type State = Map<string, lang.Constant>

function evalComp(compOp: lang.ComparisonOperator){
    switch(compOp){
        case lang.ComparisonOperator.EQ:
            return (l, r) => l === r;
        case lang.ComparisonOperator.NEQ:
            return (l, r) => l !== r;
    }
}

export function getAvailableActionRules(state: State, rules: lang.Rule[]): lang.ARule[] {
    let get = (id: string) => { let v = state.get(id); if (v === undefined) { return undefined; } return v.value; }
    function evalCond(cond: lang.Condition) {
        let evalExp = (e: lang.Expression) => e instanceof lang.Constant ? e.value : get(e.name);
        let vl = evalExp(cond.expl);
        let vr = evalExp(cond.expr);
        let f = evalComp(cond.compOperator);
        return f(vl, vr);
    }

    function getAvailableActions(rule: lang.Rule): lang.ARule[] {
        if (rule instanceof lang.ARule) {
            return [rule];
        } else
            if (rule instanceof lang.PRule) {
                if (rule.preconditions.every(evalCond)) {
                    return concatMany(rule.rules.map(getAvailableActions));
                }
                return [];
            }
        throw new Error();
    }
    return concatMany(rules.map(getAvailableActions));
}

export function applyActionRule (state: State, rule: lang.ARule, sideEffectHandler: (se: lang.SideEffect) => void) {
    rule.updates.map((upd) => state.set(upd.name, upd.constant));
    sideEffectHandler(rule.sideEffect);
}

export function evalInitial(s: State, updates: lang.Update[]) {
    updates.forEach((update) => {
        if (s.get(update.name)) {
            throw new Error(`Variable ${update.name} redefined.`);
        }
        s.set(update.name, update.constant)
    });
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

export let makeEmptyState = () => new Map() as State;
