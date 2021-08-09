module F = Format

type t = 
  | Int of int
  | Var of string

type state = 
  | S0
  | S1
  | S2
  | S3

let char_to_int c = (int_of_char c) - 48 
let char_to_str c = Char.escaped c

(* lex : char list -> t *)
let lex chars = (* write your code *)
        let rec lex_impl state chars v = 
                match state with
                |S0 ->
                                begin
                                        match chars with
                                        | h::t ->
                                                        begin
                                                                match h with
                                                                | '-' -> 
                                                                                begin
                                                                                        match lex_impl S1 t v with
                                                                                        |Int v -> Int(-v)
                                                                                        | _ -> failwith "Not a valid integer or a valid variable"
                                                                                end
                                                                | '0' .. '9' -> lex_impl S2 t (v @ [h;])
                                                                | 'a' .. 'z' -> lex_impl S3 t (v @ [h;])
                                                                | _ -> failwith "Not a valid integer or a valid variable"        
                                                        end
                                        |[] -> failwith "Not a valid integer or a valid variable"
                                end
                |S1 -> 
                                begin
                                        match chars with
                                        |h::t -> 
                                                        begin
                                                                match h with
                                                                |'0' .. '9' -> lex_impl S2 t (v @ [h;])
                                                                | _ -> failwith "Not a valid integer or a valid variable"
                                                        end
                                        |[] -> failwith "Not a valid integer or a valid variable"
                                end

                |S2 -> 
                                begin
                                        match chars with
                                        |h::t -> 
                                                        begin
                                                                match h with
                                                                |'0' .. '9' -> lex_impl S2 t (v @ [h;])
                                                                | _ -> failwith "Not a valid integer or a valid variable"
                                                        end
                                        |[] ->
                                                       begin 
                                                        let rec for_Int charlist k =
                                                                match charlist with
                                                                | h::t -> for_Int t (10 * k + (char_to_int h))
                                                                | [] -> k
                                                        in
                                                        Int (for_Int v 0)
                                                       end                 
                                end
                |S3 -> 
                        begin
                                match chars with
                                |h::t -> 
                                                begin
                                                        match h with
                                                        |'a' .. 'z' -> lex_impl S3 t (v @ [h;])
                                                        |'A' .. 'Z' -> lex_impl S3 t (v @ [h;])
                                                        |'0' .. '9' -> lex_impl S3 t (v @ [h;])
                                                        |'\'' -> lex_impl S3 t (v @ [h;])
                                                        |'_' -> lex_impl S3 t (v @ [h;])
                                                        | _ -> failwith "Not a valid integer or a valid variable"
                                                end
                                |[] -> 
                                                begin
                                                        let rec for_Var charlist k = 
                                                                match charlist with
                                                                |h::t -> for_Var t (k ^ (char_to_str h))
                                                                |[] -> k
                                                        in
                                                        Var (for_Var v "")
                                                end                    
                        end             
        in
        lex_impl S0 chars []        

let pp fmt v = 
  match v with
  | Int i -> F.fprintf fmt "Int %d" i
  | Var x -> F.fprintf fmt "Var %s" x 
