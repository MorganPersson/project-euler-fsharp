open System

let even n = n/2L
let odd n = 3L*n + 1L

let printList l = 
    l |> List.iter (fun f-> printf "%d->" f)
    printfn ""

let buildSequence n = 
    let rec f n acc = 
        let func = if (n%2L)=0L then even else odd
        if (n=1L) then 
            let result = (n::acc)
            result
        else
            let nn = func n
            f nn (n::acc)
    f n []

let longest = let rec f n acc =
                    let s = buildSequence n
                    if (n=1000000L) then
                        acc
                    else
                        let newAcc = if (List.length s > List.length acc) then 
                                        List.rev s 
                                        else 
                                        acc
                        f (n+1L) newAcc
              f 2L []


printList longest 
printfn "%d" (List.head longest)
