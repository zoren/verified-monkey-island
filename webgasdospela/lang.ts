export class Action {
    constructor(public readonly actionName: string, public readonly args: string[]) {}
}

export class VariableDeref {
    constructor(public readonly name: string) {}
}

export class Constant {
    constructor(public readonly value: string) {}
}

export type Expression = Constant | VariableDeref

export class Update {
    constructor(public readonly name: string, public readonly exp: Expression) {}
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

export class Story {
    constructor(public readonly initialState: Update[], public readonly rules: Rule[]){}
}
