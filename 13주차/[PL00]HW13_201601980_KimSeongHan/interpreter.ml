module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) (s : Store.t) : Store.value = 
        match e with
        | Num n -> NumV n
        | Bool b -> BoolV b
        | Var x -> Store.find x s
        | Add(e1,e2) ->
                       begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> NumV(n1 + n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e e1 Ast.pp_e e2)
                       end 
                        
        | Sub(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> NumV(n1 - n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid subtraction: %a - %a" Ast.pp_e e1 Ast.pp_e e2)
                        end

        |Lt(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> BoolV(n1 < n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |Gt(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> BoolV(n1 > n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid greater-than: %a > %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |Eq(e1,e2) ->
                        begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> BoolV(n1 == n2)
                                | BoolV b1, BoolV b2 -> BoolV(b1 == b2)
                                | _ , _ -> failwith (Format.asprintf "Invalid equal-to: %a == %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |And(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | BoolV b1, BoolV b2 -> BoolV(b1 && b2)
                                | _ , _ -> failwith (Format.asprintf "Invalid logical-and: %a && %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |Or(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 s in
                                let v2 = interp_e e2 s in
                                match v1,v2 with
                                | BoolV b1, BoolV b2 -> BoolV(b1 || b2)
                                | _ , _ -> failwith (Format.asprintf "Invalid logical-or: %a || %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        

(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) (s : Store.t) : Store.t =
       match stmt with
       | AssignStmt(str,e) -> 
                     begin
                             let v = interp_e e s in
                             (Store.insert str v s)
                     end  
       | IfStmt(e,true_stmts,None) -> 
                       begin
                               let v = interp_e e s in
                               let rec cal_list li acc =
                                       match li with
                                       |h::t -> cal_list t (interp_s h acc)
                                       |[] -> acc
                               in
                               match v with
                               |BoolV v -> if v = true then (cal_list true_stmts s) else s
                               | _ ->  failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)   
                       end
       | IfStmt(e,true_stmts,Some false_stmts) -> 
                       begin
                               let v = interp_e e s in
                               let rec cal_list li acc =
                                       match li with
                                       |h::t -> cal_list t (interp_s h acc)
                                       |[] -> acc
                               in
                               match v with
                               |BoolV v -> if v = true then (cal_list true_stmts s) else (cal_list false_stmts s)
                               | _ ->  failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                       end
  

(* practice & homework *)
let interp (p : Ast.program) : Store.t = 
       match p with
       | Program(li) ->
                       begin
                                let rec interp_imp li acc =
                                        match li with
                                        | h::t -> (interp_imp t (interp_s h acc))
                                        | [] -> acc
                                in
                                interp_imp li []      
                       end       
                       
