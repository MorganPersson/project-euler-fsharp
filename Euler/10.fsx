#load "util.fsx"
open Util

let p = primes 2000000 |> List.map (fun f->(int64) f)
let sum = Seq.sum p
printfn "%d" (sum)
