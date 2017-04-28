import { Nil, Cons, List, forEach, lookup} from "./list"
import * as lang from "./lang"
import * as interpreter from "./interpreter"

export type State = Map<string, lang.Constant>

let lift = (state: State) => (s: string) => {
    let c = state.get(s);
    return c ? c.value : undefined;
}

export function evalUpdates(s: State, updates: lang.Update[]) {
    updates.forEach((update) => {
        s.set(update.name, update.constant)
    });
}

export function applyActionRule (state: State, rule: lang.ARule, sideEffectHandler: (se: lang.SideEffect) => void) {
    sideEffectHandler(rule.sideEffect);    
    evalUpdates(state, rule.updates);
}

function applyActionRuleCopy (state: State, rule: lang.ARule, sideEffectHandler: (se: lang.SideEffect) => void): State {
    sideEffectHandler(rule.sideEffect);
    let sc = new Map(state);
    evalUpdates(sc, rule.updates);
    return sc;
}

export function getDeclsAsInitialState(story: lang.Story) {
    let initialDecls = interpreter.getInitialStateDecls(story);
    let initialState: State = new Map();
    initialDecls.forEach((updates) => evalUpdates(initialState, updates))
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

function shuffle<T>(array: T[]) {
  var currentIndex = array.length, temporaryValue, randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
  }
}

export function findPath(givenState: State, rules: lang.Rule[], pred: (state: interpreter.Store) => boolean) {
    let stack: WorkItem[] = [new WorkItem(givenState, Nil)];
    let visitedStates: Set<string> = new Set();

    while (true) {
        let workItem = stack.pop();
        if (!workItem) {
            console.log(`visited ${visitedStates.size} states`);
            return;
        }
        let state = workItem.state;
        let s = lift(state);
        if (pred(s)) {
            console.log(`visited ${visitedStates.size} states`);
            return workItem.path;
        }
        let availableActionRules = interpreter.getAvailableActionRules(s, rules);
        shuffle(availableActionRules);
        for (let actionRule of availableActionRules) {
            let newState = applyActionRuleCopy(state, actionRule, () => { });
            let newWorkItem = new WorkItem(newState, new Cons(actionRule.action, workItem.path));
            if (!visitedStates.has(newWorkItem.stateString)) {
                stack.push(newWorkItem);                
            }
        }
        visitedStates.add(workItem.stateString);
    }
}
