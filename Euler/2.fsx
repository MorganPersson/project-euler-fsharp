let rec fib l = 
    match l with
        |[] -> fib [2;1]
        | h::t when h < 4000000 -> fib ((h + (Seq.nth 0 t)) :: l)
        | h::t -> Seq.filter (fun f -> (f%2)=0) t |> Seq.sum 

printfn "1: %d" (fib [])  