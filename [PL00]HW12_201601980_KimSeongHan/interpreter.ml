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
       | Id str -> 
                       begin
                               let v = Store.find str s in
                               match v with
                               | FreezedV (e, s') -> (interp_e s' e)
                               | _ -> v
                       end
       | LetIn (str,e1,e2) -> interp_e (Store.insert str (interp_e s e1) s) e2
       | RLetIn (x, e1, e2) -> 
                                begin
                                        match interp_e s e1 with
                                        | ClosureV(x', e, s') ->
                                                        let rec s'' = (x, (Store.ClosureV(x', e, s''))) :: s' in
                                                        (interp_e s'' e2)                  
                                        | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp_e e1)
                                end
       | App (e1,e2) ->
                       begin
                                match interp_e s e1 with
                                | ClosureV (x,e3,s') -> (interp_e (Store.insert x (Store.FreezedV(e2, s)) s') e3)
                                | _ -> failwith (Format.asprintf "Not a function: %a" Ast.pp_e e1)

                       end

       | Fun (str,e) -> ClosureV(str,e,s)
       | Lt (e1,e2) ->
                       begin
                                let v1 = interp_e s e1 in
                                let v2 = interp_e s e2 in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> if n1 < n2 then ClosureV("x",Fun("y",Id("x")),[]) else ClosureV("x",Fun("y",Id("y")),[])
                                | _, _ -> failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e e1 Ast.pp_e e2)
                       end 
  
(* practice & homework *)
let interp (p : Ast.fvae) : Store.value = 
        match p with
        |Prog expr -> interp_e [] expr

