type expression =
  | Var of string
  | Number of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression
  | Unit
  | True | False

type lexem = 
  | End | LNothing
  | LNumber of string
  | LVar of string
  | LOp of int*string (*the int is the prority, higher for more priority*)
  | LFun
  | LRec | LArrow
  | LLet | LIn
  | LIf | LThen | LElse
  | LLeftPar | LRightPar | LComma
  | LTrue | LFalse
  | LUnit
  | LSemicolon
  | E | F of int | G | S
