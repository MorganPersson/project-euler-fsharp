module Util
    open System.Diagnostics
    open System.Threading

    let isPrime n =
        if (n=1) then 
            true
        else
            let rec isPrime n d =
                match d with
                    | z when (n=d) -> true
                    | z when (n%d)=0 -> false
                    | _ -> isPrime n (d+1)
            isPrime n 2
    
    let primes n =
        let arr = seq {for i in 0..n do if (i%2)=1 || i=2 then yield i else yield 0} |> Array.ofSeq
        let limit = int (sqrt (float n)) 
        for i in [3..2..(limit+1)] do
            for j in [(i*i)..i..n] do
                arr.[j]<-0
        
        Seq.filter (fun f-> f>1) arr |> List.ofSeq
