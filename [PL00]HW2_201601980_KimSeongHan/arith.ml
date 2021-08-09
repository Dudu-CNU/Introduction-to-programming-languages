type unop = Neg
type binop = Add | Sub | Mul | Div
type exp = 
        |Constant of int
        |Unary of unop * exp
        |Binary of exp * binop * exp

let uncal op exp =
        match op with
        | Neg -> -exp


let bical exp1 op exp2 =
       match op with
       | Add -> exp1 + exp2
       | Sub -> exp1 - exp2
       | Mul -> exp1 * exp2
       | Div -> exp1 / exp2
       
let rec eval exp =
       match exp with
       | Constant(h) -> h
       | Unary(h,t) -> (uncal h (eval t))
       | Binary(h,m,t) -> (bical (eval h) m (eval t))
