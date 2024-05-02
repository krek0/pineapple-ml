type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression

type lexem = 
  | End | LNothing
  | LConst of string
  | LVar of string
  | LOp0 of string
  | LOp1 of string
  | LFun
  | LRec | LArrow
  | LLet | LIn
  | LIf | LThen | LElse
  | LLeftPar | LRightPar | LComma
  | E | F | G | H | S
