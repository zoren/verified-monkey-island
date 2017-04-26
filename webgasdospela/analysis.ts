import { Nil, Cons, List, forEach, lookup} from "./list"
import * as lang from "./lang"
import * as interpreter from "./interpreter"

type State = List<[string, lang.Constant]>

let lift = (state: State) => (s: string) => {
    let c = lookup(s, state);
    return c ? c.value : undefined;
}

function evalUpdates(s: State, updates: lang.Update[]): State {   
    updates.forEach((update) => {
        s = new Cons([update.name, update.constant], s);
    });
    return s;
}

function applyActionRule (state: State, rule: lang.ARule, sideEffectHandler: (se: lang.SideEffect) => void): State {
    sideEffectHandler(rule.sideEffect);
    return evalUpdates(state, rule.updates);
}

function printState(state: State){
    console.log("{");
    forEach((t) => {console.log(t[0], t[1].value)})(state);
    console.log("}");
}

export function findPath(story: lang.Story, pred: (state: interpreter.State) => boolean) {
    let rules = interpreter.getRulesDecls(story);
    function evalRec(state: State) {
        let s = lift(state);
        if(pred(s)){
            throw new Error("found a path");
        }
        let availableActionRules = interpreter.getAvailableActionRules(s, rules);
        for(let action of availableActionRules) {
            let s2 = applyActionRule(state, action, () => {});
            evalRec(s2);
        }
    }

    function stateToString(state: State){
        let m = new Map<string, string>();
        let f = (tup:[string, lang.Constant]) =>{
            let v = m.get(tup[0]);
            if(!v){
                m.set(tup[0], tup[1].value)
            }
        }
        forEach(f)(state)
        let ar = Array.from(m.entries());
        ar.sort((a, b) => a[0].localeCompare(b[0]));
        return ar.map(([k,v]) => k + v).join();
    }

    let stack: {state: State, path: List<lang.Action>}[] = []
    let visitedStates: Set<string> = new Set();

    function evalNoRec(): List<lang.Action> | undefined {
        while (true) {
            let workItem = stack.pop();
            if (!workItem) {
                return;
            }
            let state = workItem.state;
            let stateString = stateToString(state);
            if(visitedStates.has(stateString)){
                continue;
            }
            let s = lift(state);
            if (pred(s)) {
                return workItem.path;
            }
            let availableActionRules = interpreter.getAvailableActionRules(s, rules);
            for (let actionRule of availableActionRules) {
                let s2 = applyActionRule(state, actionRule, () => { });
                stack.push({state: s2, path: new Cons(actionRule.action, workItem.path) } );
            }
            visitedStates.add(stateString);
        }
    }

    let initialDecls = interpreter.getInitialStateDecls(story);
    let initialState: State = Nil;
    for(let upd of initialDecls){
        initialState = evalUpdates(initialState, upd);
    }

    stack.push({state: initialState, path: Nil});
    return evalNoRec();
}
