type exp =
  | Int of str
  | Float of str
  | Sum of exp * exp
  | Sub of exp * exp
  | Prod of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | Sumf of exp * exp
  | Subf of exp * exp
  | Prodf of exp * exp
  | Iof of exp
  | Foi of exp


let ast l =
  
