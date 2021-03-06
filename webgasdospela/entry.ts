import * as list from "./list"
import * as parser from "./parser"
import * as lang from "./lang"
import * as interpreter from "./interpreter"

import * as analysis from "./analysis"
import * as infer from "./infer"
    
let story: lang.Story | undefined;

let currentState = new Map();

function print(otmm: infer.OneToManyMap<string, string>){
    for(let comp of otmm.keys()){
        console.log(comp, otmm.get(comp));
    }
}

export function loadStory() {
    let current = <HTMLTextAreaElement>document.getElementById("story-text");    
    var result = parser.story.parse(current.value);
    if(result.status){
        story = result.value;
        let {compMap, updatesMap} = infer.getStoryDomains(story);
        print(compMap);
        print(updatesMap);

        let initialStateDecls = interpreter.getInitialStateDecls(story);
        let lState = analysis.lift(currentState);
        for(let updates of initialStateDecls){
            analysis.evalUpdates(currentState, updates);
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
                button.onclick = () => { actionRules.forEach((actionRule) => analysis.applyActionRule(currentState, actionRule, handler)); listAvailableActions() };
                availableActions.appendChild(button);
                availableActions.appendChild(document.createElement("br"));
            });
            inventory.innerHTML = "";
            currentState.forEach((v, k) => {
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
    xhr.onload = ((ev) => { 
        if(xhr.status !== 200){
            throw new Error("could not find " + file);
        }
        console.log("loaded " + file);
        current.value = xhr.responseText
    });
}

function clearPathDisplay(){
    let d = <HTMLTextAreaElement>document.getElementById("actions-textarea");        
    d.value = "";
}

function showPath(path: list.List<lang.Action>) {
    let ar: lang.Action[] = [];
    list.forEach((a) => ar.push(a))(path);
    ar.reverse();
    console.log(`path is ${ar.length} long`);
    clearPathDisplay();
    let d = <HTMLTextAreaElement>document.getElementById("actions-textarea");        
    d.rows = ar.length;
    let s: string[] = [];
    ar.forEach((a) => s.push(a.toString()));
    d.value = s.join("\n");
}

function getPredicate() {
    let predicateInput = <HTMLInputElement>document.getElementById("predicate-input");
    let s = predicateInput.value;
    let v = parser.comparisons.parse(s);
    if (!v.status) {
        throw new Error("could not parse");
    }
    let comparisons = v.value;
    return (s: interpreter.Store) => interpreter.evalConds(s, comparisons);
}

function getStory(){
    if(!story){
        loadStory();
    }
    if(!story){
        throw new Error("story not loaded, could not analyse");
    }
    return story;
}

function analyse(startingState: analysis.State) {
    let story = getStory();
    let pred = getPredicate();
    let rules = interpreter.getRulesDecls(story);
    let foundPath = analysis.findPath(startingState, rules, pred);
    if(foundPath){
        showPath(foundPath);
    }else{
        console.log("did not find path");
    }
}

export function analyseInitial() {
    clearPathDisplay();
    let story = getStory();    
    let initialState = analysis.getDeclsAsInitialState(story);
    analyse(initialState);
}

export function analyseCurrent() {
    clearPathDisplay();
    let story = getStory();    
    analyse(new Map(currentState));
}
