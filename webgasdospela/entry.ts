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
        current.innerText = "Welcome!"
        let availableActions = <HTMLDivElement>document.getElementById("available-actions");
        let handler = (se: lang.SideEffect) => {if(se){current.innerText = se.printText;}}
        let rules = Array.from(s.rules);
        let listAvailableActions = () => {
            availableActions.innerHTML = ""
            let actions = interpreter.getAvailableActionRules(state, rules);
            actions.forEach((actionRule) => {
                let button = document.createElement("button");
                button.innerText = actionRule.action.toString();
                button.onclick = () => { interpreter.applyActionRule(state, actionRule, handler); listAvailableActions() }
                availableActions.appendChild(button);
            });
        }
        listAvailableActions();
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
