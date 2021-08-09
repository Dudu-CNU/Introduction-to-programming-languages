module F = Format

(* practice & homework *)
let rec interp_e (s : Store.t) (e : Ast.expr) : Store.value = 
        match e with
        | Num n -> NumV n  
        | Add (e1,e2) -> 
                        begin
                                let exp1 = interp_e s e1 in
                                let exp2 = interp_e s e2 in
                                match exp1,exp2 with
                                | NumV n1, NumV n2 -> NumV(n1 + n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        | Sub (e1,e2) ->
                        begin
                                let exp1 = interp_e s e1 in
                                let exp2 = interp_e s e2 in
                                match exp1,exp2 with
                                | NumV n1, NumV n2 -> NumV(n1 - n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid subtraction: %a - %a" Ast.pp_e e1 Ast.pp_e e2)
                        end 
        | Id str -> Store.find str s
        | LetIn (str,e1,e2) -> interp_e (Store.insert str (interp_e s e1) s) e2
        | App (e1,e2) -> 
                        begin
                                let v1 = interp_e s e2 in
                                match interp_e s e1 with
                                | ClosureV (x,e3,s') -> interp_e (Store.insert x v1 s') e3
                                | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp_e e1)
                                                                
                        end

        | Fun (str,e) -> ClosureV(str,e,s)

(* practice & homework *)
let interp (p : Ast.fvae) : Store.value =
       match p with 
       | Prog expr -> interp_e [] expr  
