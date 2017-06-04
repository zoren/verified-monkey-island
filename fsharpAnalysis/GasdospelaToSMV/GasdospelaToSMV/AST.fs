namespace GasdospelaToSMV

module AST =

    type ActionName = string
    type Action = ActionName * string list

    type Update<'Var, 'Val> = 'Var * 'Val

    type Exp<'Var, 'Val> =
        | VariableDeref of 'Var
        | Constant of 'Val

    type SideEffect = string option

    type ComparisonOperator = EQ | NEQ

    type Condition<'Var, 'Val> = Exp<'Var, 'Val> * ComparisonOperator * Exp<'Var, 'Val>

    type Rule<'Var, 'Val> =
        | ARule of Action * Update<'Var, 'Val> list * SideEffect
        | PRule of Condition<'Var, 'Val> list * Rule<'Var, 'Val> list

    type Decl<'Var, 'Val> =
        | InitBlock of Update<'Var, 'Val> list
        | Rule of Rule<'Var, 'Val>
