import { Nil, Cons, List, forEach, lookup} from "./list"
import * as lang from "./lang"
import * as interpreter from "./interpreter"

export type State = Map<string, lang.Constant>

let lift = (state: State) => (s: string) => {
    let c = state.get(s);
    return c ? c.value : undefined;
}

function evalUpdatesCopy(s: State, updates: lang.Update[]): State {
    let sc = new Map(s);
    updates.forEach((update) => sc.set(update.name, update.constant));
    return sc;
}

function applyActionRule (state: State, rule: lang.ARule, sideEffectHandler: (se: lang.SideEffect) => void): State {
    sideEffectHandler(rule.sideEffect);
    return evalUpdatesCopy(state, rule.updates);
}

export function getDeclsAsInitialState(story: lang.Story) {
    let initialDecls = interpreter.getInitialStateDecls(story);
    let initialState: State = new Map();
    initialDecls.forEach((updates) =>
        updates.forEach((update) =>
            initialState.set(update.name, update.constant)
        ));
    return initialState;
}

function stateToString(state: State){
    let ar = Array.from(state.entries());
    ar.sort((a, b) => a[0].localeCompare(b[0]));
    return ar.map(([k,v]) => k + v).join();
}

class WorkItem {
    public readonly stateString: string;
    constructor(public readonly state: State, public readonly path: List<lang.Action>) {
        this.stateString = stateToString(state);
    }
}

export function findPath(givenState: State, rules: lang.Rule[], pred: (state: interpreter.State) => boolean) {
    let stack: WorkItem[] = [new WorkItem(givenState, Nil)];
    let visitedStates: Set<string> = new Set();

    while (true) {
        let workItem = stack.pop();
        if (!workItem) {
            return;
        }
        let state = workItem.state;
        let s = lift(state);
        if (pred(s)) {
            return workItem.path;
        }
        let availableActionRules = interpreter.getAvailableActionRules(s, rules);
        for (let actionRule of availableActionRules) {
            let newState = applyActionRule(state, actionRule, () => { });
            let newWorkItem = new WorkItem(newState, new Cons(actionRule.action, workItem.path));
            if (!visitedStates.has(newWorkItem.stateString)) {
                stack.push(newWorkItem);                
            }
        }
        visitedStates.add(workItem.stateString);
    }
}
