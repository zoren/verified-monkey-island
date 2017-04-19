import * as parser from "./parser"
import * as lang from "./lang"
import * as interpreter from "./interpreter"

export function loadStory() {
    let current = <HTMLTextAreaElement>document.getElementById("story-text");    
    var result = parser.story.parse(current.value);
    if(result.status){
        let s = result.value;
        let state = interpreter.evalInitial(s.initialState);

        let current = <HTMLDivElement>document.getElementById("current-message");
        let availableActions = <HTMLDivElement>document.getElementById("available-actions");
        let inventory = <HTMLDivElement>document.getElementById("inventory");
        let handler = (se: lang.SideEffect) => {if(se){current.innerText = se.printText;}}
        let rules = Array.from(s.rules);
        let listAvailableActions = () => {
            availableActions.innerHTML = ""
            let actions = interpreter.getAvailableActionRules(state, rules);

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
                button.onclick = () => { actionRules.forEach((actionRule) => interpreter.applyActionRule(state, actionRule, handler)); listAvailableActions() };
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
