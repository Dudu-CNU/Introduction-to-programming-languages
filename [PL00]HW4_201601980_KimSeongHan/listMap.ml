module F = Format

type t = (string * string) list 

let empty = []

let add key value map = 
        ((key,value) :: map)
        
let rec find key map = 
        match map with
        |[] -> failwith "No such key exists"
        |(k,v) :: t -> if key = k then v else (find key t)   

let erase key map = 
        let l = List.length map in
        (*erase 함수 : not tail-recursion*)
        let rec erase_2 key map =
                match map with
                |[] -> []
                |(k,v) :: t -> if key = k then erase_2 key t 
                               else ((k,v) :: erase_2 key t)
        in
        let newMap = erase_2 key map in
        (* erase 함수 : tail-recursion
        let rec erase_2 key map acc =
                match map with
                |[] -> acc
                |(k,v)::t -> if key = k then (erase_2 key t acc)
                             else (erase_2 key t (acc @ [(k,v);]))
        in
        let newMap = erase_2 key map [] in
        *)
        if List.length newMap = l then failwith "No such key exists" else newMap
        
                  
let print_map fmt map = 
    let rec print_map_impl map = 
            match map with
            | [] -> ()
            | (k, v) :: t -> 
                     let () = F.fprintf fmt "(%s, %s) " k v in
                     print_map_impl t  
    in
          
    let () = F.fprintf fmt "[ " in
    let () = print_map_impl map in
    F.fprintf fmt "]"
