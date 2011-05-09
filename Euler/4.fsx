open System
open System.Collections.Generic

let findLargestPalindrom =
    let a = [100..999]
    let b = [100..999]
    
    let reverse i = 
        Int32.Parse(new string (i.ToString().ToCharArray() |> Array.rev))

    let x = [ for n in a do
                for m in b do
                    yield n*m ]

    let l = List.filter (fun f -> (f = reverse f)) x |> List.sort |> List.rev
    List.nth l 0

printfn "%d" (findLargestPalindrom) 
