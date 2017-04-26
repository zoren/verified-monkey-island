import * as list from "./list"
import * as parser from "./parser"
import * as lang from "./lang"
import * as interpreter from "./interpreter"

function evalInitial(s: Map<string, lang.Constant>, updates: lang.Update[]) {
    updates.forEach((update) => {
        s.set(update.name, update.constant)
    });
}

function applyActionRule (state: Map<string, lang.Constant>, rule: lang.ARule, sideEffectHandler: (se: lang.SideEffect) => void) {
    rule.updates.map((upd) => state.set(upd.name, upd.constant));
    sideEffectHandler(rule.sideEffect);
}

let liftState = (map: Map<string, lang.Constant>) => ( (v: string) => {
        let c = map.get(v);
        return c ? c.value : undefined;
    });

import * as analysis from "./analysis"
    
let story: lang.Story | undefined;

export function loadStory() {
    let current = <HTMLTextAreaElement>document.getElementById("story-text");    
    var result = parser.story.parse(current.value);
    if(result.status){
        story = result.value;
        let initialStateDecls = interpreter.getInitialStateDecls(story);
        let state = new Map();
        let lState = liftState(state);
        for(let updates of initialStateDecls){
            evalInitial(state, updates);
        }

        let current = <HTMLDivElement>document.getElementById("current-message");
        let availableActions = <HTMLDivElement>document.getElementById("available-actions");
        let inventory = <HTMLDivElement>document.getElementById("inventory");
        let handler = (se: lang.SideEffect) => {if(se){current.innerText = se.printText;}}
        let rules = interpreter.getRulesDecls(story);
        let listAvailableActions = () => {
            availableActions.innerHTML = ""
            let actions = interpreter.getAvailableActionRules(lState, rules);

            let dedub = new Map<string, lang.ARule[]>();
            actions.forEach((action) => {
                let k = action.action.toString();
                let v = dedub.get(k);
                if(!v){
                    v = [];
                    dedub.set(k, v);
                }
                v.push(action);
            })

            dedub.forEach((actionRules, actionString) => {
                let button = document.createElement("button");
                button.innerText = actionString;
                button.onclick = () => { actionRules.forEach((actionRule) => applyActionRule(state, actionRule, handler)); listAvailableActions() };
                availableActions.appendChild(button);
                availableActions.appendChild(document.createElement("br"));
            });
            inventory.innerHTML = "";
            state.forEach((v, k) => {
                if (v.value === "Inv") {
                    let e = document.createElement("div");
                    e.innerText = k.endsWith(".loc") ? k.substr(0, k.length - 4) : k;
                    inventory.appendChild(e);
                }
            })
        }
        listAvailableActions();
    }else{
        console.log(result);
    }
}

export function loadMI2part1() {
    const file = "MI2part1.core";
    let current = <HTMLTextAreaElement>document.getElementById("story-text");    
    let xhr = new XMLHttpRequest();
    xhr.open("GET", file);
    xhr.send();
    xhr.onreadystatechange = ((ev) => { 
        if(xhr.status !== 200){
            throw new Error("could not find " + file);
        }
        console.log("loaded " + file);
        current.value = xhr.responseText
    });
}

export function analyse() {
    let predicateInput = <HTMLInputElement>document.getElementById("predicate-input");
    let s = predicateInput.value;
    let v = parser.comparisons.parse(s);
    if(!v.status){
        return console.error("could not parse", v);
    }
    if(!story){
        loadStory();
    }
    if(!story){
        return console.error("story not loaded, could not analyse");
    }
    let comparisons = v.value;
    let pred = (s: interpreter.State) => interpreter.evalConds(s, comparisons);
    let initialState = analysis.getDeclsAsInitialState(story);
    let rules = interpreter.getRulesDecls(story);
    let foundPath = analysis.findPath(initialState, rules, pred);
    if(foundPath){
        let ar: lang.Action[] = [];
        list.forEach((a) => ar.push(a))(foundPath);
        ar.reverse();
        let d = <HTMLTextAreaElement>document.getElementById("actions-textarea");        
        d.innerHTML = "";
        d.rows = ar.length;
        ar.forEach((a) => console.log(d.value += a.toString() + "\n"));
    }else{
        console.log("did not find path");
    }
}
