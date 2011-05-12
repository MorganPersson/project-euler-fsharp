let isPythagoras p =
    let a,b,c = p
    (a*a + b*b = c*c)

let problem = 
    [for a in 1..997 do
        printfn "a: %d" a
        for b in (a+1)..998 do
            if (a+b+b) < 1000 then
                let c = 1000 - a - b
                if (isPythagoras (a,b,c)) then
                    yield (a,b,c)
    ] |> Seq.nth 0

let a,b,c = problem
printfn "%d * %d * %d = %d" a b c (a*b*c)
