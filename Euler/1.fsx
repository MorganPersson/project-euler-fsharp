let sum = 
    seq { 0..999 }
    |> Seq.map (fun n -> if (n%3)=0 || (n%5)=0 then n else 0 )
    |> Seq.sum

printfn "1: %d" sum