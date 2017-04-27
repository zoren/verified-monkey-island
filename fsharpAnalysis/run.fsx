#load "parser.fsx"
open Parser
printfn "%A" <| parseString """pickUp(shovel) >> {shovel.loc := Inv} & { "You pick up a shovel" }"""
