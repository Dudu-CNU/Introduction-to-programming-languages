let split il =
        let rec split_tail il acc1 acc2 = 
                match il with
                | [] -> (acc1, acc2) 
                | (a,b) :: t -> (split_tail t (acc1 @ [a;]) (acc2 @ [b;]))
        in
        split_tail il [] []

let combine il1 il2 = 
        let rec combine_tail  il1 il2 acc =   
                match il1,il2 with
                | [],[] -> acc
                | h1::t1, h2::t2 -> combine_tail t1 t2 (acc @ [(h1,h2);])
                | ([],_::_) | (_::_,[]) -> failwith "Unsupported"
        in 
        combine_tail il1 il2 []         
        
