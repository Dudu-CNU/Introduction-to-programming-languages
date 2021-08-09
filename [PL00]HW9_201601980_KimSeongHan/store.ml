module F = Format

type t = (string * int) list

let empty = []

let insert x n s = 
        ((x,n) :: s)

let rec find x s = 
        match s with
        |[] -> failwith ("Free identifier " ^ x)
        |(k,v)::t -> if k = x then v else (find x t)

let pp fmt s = 
  let rec pp_impl fmt s = 
    match s with
    | [] -> F.fprintf fmt "]"
    | (x, n) :: t -> F.fprintf fmt "(%s, %d) %a" x n pp_impl t
  in
  F.fprintf fmt "[ %a" pp_impl s
