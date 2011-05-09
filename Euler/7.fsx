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

let tusenEtt = Seq.initInfinite (fun i->i+1) |> Seq.filter isPrime |> Seq.nth 10001

printfn "%d" tusenEtt