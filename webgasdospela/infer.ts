import * as lang from "./lang"

export class OneToManyMap<TKey, TValue> {
    private map: Map<TKey, TValue[]>;
    constructor() {
        this.map = new Map<TKey, TValue[]>();
    }
    public get(key: TKey): TValue[] {
        let v = this.map.get(key);
        if (v) {
            return v;
        }
        return [];
    }
    public set(key: TKey, value: TValue) {
        let ar = this.map.get(key);
        if (!ar) {
            ar = [];
            this.map.set(key, ar);
        }
        if (ar.indexOf(value) === -1) { ar.push(value); }
    }
    public keys(): IterableIterator<TKey> {
        return this.map.keys();
    }
}

export function getStoryDomains(story: lang.Story) {
    let compMap = new OneToManyMap<string, string>();
    let updatesMap = new OneToManyMap<string, string>();
    let visitConds = (conds: lang.Condition[]) => {
        conds.forEach((cond) => {
            if (!(cond.expl instanceof lang.VariableDeref && cond.expr instanceof lang.Constant)) {
                throw new Error("only expecting variables compared to constants");
            }
            compMap.set(cond.expl.name, lang.ComparisonOperator[cond.compOperator] + " " + cond.expr.value);
        })
    }
    let visitUpdates = (updates: lang.Update[]) => {
        updates.forEach((update) => updatesMap.set(update.name, update.constant.value))
    }
    let visitRule = (rule: lang.Rule) => {
        if(rule instanceof lang.ARule){
            visitUpdates(rule.updates);
        }
        if(rule instanceof lang.PRule){
            visitConds(rule.preconditions)
            rule.rules.forEach(visitRule)
        }
    }
    let visitDecl = (decl: lang.StoryDecl) => {
        if(decl instanceof lang.InitBlock){
            return visitUpdates(decl.updates)
        }
        visitRule(decl);
    }
    story.decls.forEach(visitDecl);
    return {compMap, updatesMap}
}
