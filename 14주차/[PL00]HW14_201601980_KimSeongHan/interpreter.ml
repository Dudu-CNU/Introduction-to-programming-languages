module F = Format

(* practice & homework *)
let rec interp_e (e : Ast.expr) ((env, mem) : Env.t * Mem.t) : Mem.value = 
        match e with
        | Num n -> NumV n
        | Bool b -> BoolV b
        | Var x -> Mem.find (Env.find x env) mem
        | Ref x -> AddressV (Env.find x env)
        | Deref x -> 
                        begin
                                let a = Mem.find (Env.find x env) mem in
                                match a with
                                | AddressV ad -> (Mem.find ad mem)
                                | _ -> failwith (Format.asprintf "Not a memory address : %a" Mem.pp_v a)
                        end
        | Add(e1,e2) ->
                       begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> NumV (n1 + n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid addition: %a + %a" Ast.pp_e e1 Ast.pp_e e2)
                       end 
                        
        | Sub(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> NumV(n1 - n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid subtraction: %a - %a" Ast.pp_e e1 Ast.pp_e e2)
                        end

        |Lt(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> BoolV(n1 < n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid less-than: %a < %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |Gt(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> BoolV(n1 > n2)
                                | _ , _ -> failwith (Format.asprintf "Invalid greater-than: %a > %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |Eq(e1,e2) ->
                        begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | NumV n1, NumV n2 -> BoolV(n1 == n2)
                                | BoolV b1, BoolV b2 -> BoolV(b1 == b2)
                                | _ , _ -> failwith (Format.asprintf "Invalid equal-to: %a == %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |And(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | BoolV b1, BoolV b2 -> BoolV(b1 && b2)
                                | _ , _ -> failwith (Format.asprintf "Invalid logical-and: %a && %a" Ast.pp_e e1 Ast.pp_e e2)
                        end
        |Or(e1,e2) -> 
                        begin
                                let v1 = interp_e e1 (env,mem) in
                                let v2 = interp_e e2 (env,mem) in
                                match v1,v2 with
                                | BoolV b1, BoolV b2 -> BoolV(b1 || b2)
                                | _ , _ -> failwith (Format.asprintf "Invalid logical-or: %a || %a" Ast.pp_e e1 Ast.pp_e e2)
                        end

(* practice & homework *)
let rec interp_s (stmt : Ast.stmt) ((env, mem) : Env.t * Mem.t) : Env.t * Mem.t = 
        match stmt with
        | IfStmt(e,true_stmts,None) ->
                       begin
                               let v = interp_e e (env, mem) in
                               let rec cal_list li (e, m) =
                                       match li with
                                       |h::t -> cal_list t (interp_s h (e, m))
                                       |[] -> (e, m)
                               in
                               match v with
                               |BoolV v -> if v = true then (cal_list true_stmts (env, mem)) else (env, mem)
                               | _ ->  failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                       end

        | IfStmt(e,true_stmts,Some false_stmts) ->
                       begin
                               let v = interp_e e (env, mem) in
                               let rec cal_list li (e, m) =
                                       match li with
                                       |h::t -> cal_list t (interp_s h (e, m))
                                       |[] -> (e, m)
                               in
                               match v with
                               |BoolV v -> if v = true then (cal_list true_stmts (env, mem)) else (cal_list false_stmts (env, mem))
                               | _ ->  failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                       end

        | VarDeclStmt(x) -> 
                       begin
                               let check = Env.mem x env in
                               if check = true then failwith (Format.asprintf "%s is already declared." x) else ((Env.insert x (Env.new_address ()) env), mem)
                       end

        | StoreStmt(e1,e2) ->
                       begin
                                let a = interp_e e1 (env, mem) in
                                let v = interp_e e2 (env, mem) in
                                match a with
                                | AddressV ad -> (env, (Mem.insert ad v mem))
                                | _ -> failwith (Format.asprintf "Not a memory address : %a" Ast.pp_e e1)
                       end

        | WhileStmt(e, li) -> 
                        begin
                                let v = interp_e e (env, mem) in
                                let rec cal_list li (ee, mm) =
                                      match li with
                                      | h::t -> cal_list t (interp_s h (ee, mm))
                                      | [] -> (ee, mm)
                                in
                                match v with
                                | BoolV v -> if v = true then interp_s (WhileStmt (e,li)) (cal_list li (env, mem)) else (env, mem)
                                | _ -> failwith (Format.asprintf "Not a boolean : %a" Ast.pp_e e)
                        end
  
(* practice & homework *)
let interp (p : Ast.program) : Env.t * Mem.t =
       match p with
       | Program(li) ->
                       begin
                                let rec interp_imp li (e, m) =
                                        match li with
                                        | h::t -> (interp_imp t (interp_s h (e, m)))
                                        | [] -> (e, m)
                                in
                                interp_imp li ([], [])      
                       end    
 
