module F = Format

(* practice *)
let rec interp (e : Ast.ae) : int =
       match e with
       | Num n -> n
       | Neg e -> (-1) * (interp e)
       | Add (e1,e2) -> (interp e1) + (interp e2)
       | Sub (e1,e2) -> (interp e1) - (interp e2)   
