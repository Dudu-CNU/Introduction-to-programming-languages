module F = Format

let rec interp_e (fenv : FEnv.t) (s : Store.t) (e : Ast.expr) : int = 
        match e with
        | Num n -> n
        | Add (e1,e2) -> (interp_e fenv s e1) + (interp_e fenv s e2)
        | Sub (e1,e2) -> (interp_e fenv s e1) - (interp_e fenv s e2)
        | Id x -> Store.find x s
        | LetIn (x,e1,e2) -> interp_e fenv (Store.insert x (interp_e fenv s e1) s) e2
        | FCall (func,expr) -> 
                                begin
                                        match (FEnv.find func fenv) with
                                        |(param,body) -> 
                                                        begin
                                                                let rec storeData_impl param expr s=                  
                                                                        match param, expr with
                                                                        | para::t1, exp::t2 -> storeData_impl t1 t2 (Store.insert para (interp_e fenv s exp) s)     
                                                                        | ([], _::_)|(_::_, []) -> failwith "Arity mismatched"
                                                                        | ([],[]) -> s
                                                                in
                                                                interp_e fenv (storeData_impl param expr s) body
                                                        end

                                end

let interp_d (fenv : FEnv.t) (fd : Ast.fundef) : FEnv.t = 
           match fd with
           | FDef (str1, plist, expr) -> FEnv.insert str1 plist expr fenv

(* practice *)
let interp (p : Ast.f1vae) : int = 
           match p with
           | Prog (fundef,expr) -> 
                                begin
                                        let rec interp_dimpl fundef acc=
                                                match fundef with
                                                | [] -> acc
                                                | h::t -> interp_dimpl t (interp_d acc h)
                                        in
                                        interp_e (interp_dimpl fundef []) [] expr
                                end 
