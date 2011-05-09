open System
open System.Diagnostics

let answer = 2*2*2*2*3*3*5*7*11*13*17*19

let rec isDivisibleByAllBelow i div = 
    match div with
        | 1 -> true
        | x when(i%div)>0 -> false
        | _ -> isDivisibleByAllBelow i (div-1)

let f = Seq.initInfinite (fun i -> 20 + 2*i) |> Seq.skipWhile (fun f -> (isDivisibleByAllBelow f 20)=false) |> Seq.nth 0

printfn "%d" (f)
printfn "%d" (answer)