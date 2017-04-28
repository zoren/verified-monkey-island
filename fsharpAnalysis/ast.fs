namespace AST

type ActionName = string
type Action = ActionName * string list

type Update = string * string

type Exp =
    | VariableDeref of string
    | Constant of string

type SideEffect = string option

type ComparisonOperator = EQ | NEQ

type Condition = Exp * ComparisonOperator * Exp

type Rule =
    | ARule of Action * Update list * SideEffect
    | PRule of Condition list * Rule list

type Decl =
    | InitBlock of Update list
    | Rule of Rule
