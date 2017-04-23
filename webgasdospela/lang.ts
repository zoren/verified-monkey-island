export class Action {
    constructor(public readonly actionName: string, public readonly args: string[]) {}
    toString(){
        return `${this.actionName}(${this.args.join(', ')})`
    }
}

export class VariableDeref {
    constructor(public readonly name: string) {}
    toString(){
        return this.name;
    }    
}

export class Constant {
    constructor(public readonly value: string) {}
    toString(){
        return this.value;
    }
}

export class Update {
    constructor(public readonly name: string, public readonly constant: Constant) {}
}

export class PrintSideEffect {
    constructor(public readonly printText: string) {}
}

export type SideEffect = PrintSideEffect | undefined;

export class ARule {
    constructor(public readonly action: Action,
                public readonly updates: Update[],
                public readonly sideEffect: SideEffect) {}
}

export enum ComparisonOperator {
    EQ, NEQ
}

export type Expression = Constant | VariableDeref

export class Condition {
    constructor(public readonly expl: Expression,
                public readonly compOperator: ComparisonOperator,
                public readonly expr: Expression) {}
}

export type Rule = ARule | PRule

export class PRule {
    constructor(public readonly preconditions: Condition[],
                public readonly rules: Rule[]) {}
}

export class InitBlock {
    constructor(public readonly updates: Update[]){}
}

export type StoryDecl = InitBlock | Rule;

export class Story {
    constructor(public readonly decls: StoryDecl[]){}
}
