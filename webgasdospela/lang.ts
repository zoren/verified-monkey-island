class Action {
    constructor(public readonly actionName: string, public readonly args: string[]) {}
}

class VariableDeref {
    constructor(public readonly name: string) {}
}

class Constant {
    constructor(public readonly value: string) {}
}

type Expression = Constant | VariableDeref

class Update {
    constructor(public readonly name: string, public readonly exp: Expression) {}
}

class PrintSideEffect {
    constructor(public readonly printText: string) {}
}

type SideEffect = PrintSideEffect | undefined;

class ARule {
    constructor(public readonly action: Action,
                public readonly updates: Update[],
                public readonly sideEffect: SideEffect) {}
}

enum ComparisonOperator {
    EQ, NEQ
}

class Condition {
    constructor(public readonly expl: Expression,
                public readonly compOperator: ComparisonOperator,
                public readonly expr: Expression) {}
}

type Rule = ARule | PRule

class PRule {
    constructor(public readonly preconditions: Condition[],
                public readonly rules: Rule[]) {}
}

class Story {
    constructor(public readonly initialState: Update[], public readonly rules: Rule[]){}
}
