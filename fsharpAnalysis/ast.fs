type ActionName = string
type Action = ActionName * string[]

type Update = string * string

type Exp =
    | VariableDeref of string
    | Constant of string

type SideEffect = string option

type ARule =
    Action * Update[] * SideEffect

type ComparisonOperator = EQ | NEQ

type Condition = Exp * ComparisonOperator * Exp

type Rule =
    | ARule of Action * Update[] * SideEffect
    | PRule of Condition[] * Rule
