import * as lang from "./lang"
import * as interpreter from "./interpreter"

class Nil{}

class Cons<T>{
    constructor(public readonly head: T, public readonly tail: List<T>) {}
}

type List<T> = Nil | Cons<T>

function lookup<K, T>(k: K, list: List<[K, T]>) {
    while(list instanceof Cons){
        if(k === list.head[0]){
            return list.head[1];
        }
        list = list.tail;
    }
}

function forEach<T>(f: (T) => void) {
    let rec =
        (list: List<T>) => {
            if (list instanceof Cons) {
                f(list.head);
                rec(list.tail);
            }
        }
    return rec;
}

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

    let stack: State[] = []
    let visitedStates: Set<string> = new Set();

    function evalNoRec() {
        while (true) {
            let state = stack.pop();
            if (!state) {
                console.log("completed evaluation of stack");
                return;
            }
            let stateString = stateToString(state);
            if(visitedStates.has(stateString)){
                continue;
            }
            let s = lift(state);
            if (pred(s)) {
                return console.log("found a path");
            }
            let availableActionRules = interpreter.getAvailableActionRules(s, rules);
            for (let action of availableActionRules) {
                let s2 = applyActionRule(state, action, () => { });
                stack.push(s2);
            }
            visitedStates.add(stateString);
        }
    }

    let initialDecls = interpreter.getInitialStateDecls(story);
    let initialState: State = Nil;
    for(let upd of initialDecls){
        initialState = evalUpdates(initialState, upd);
    }

    stack.push(initialState);
    evalNoRec();
}
