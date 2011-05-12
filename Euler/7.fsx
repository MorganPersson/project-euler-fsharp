#load "util.fsx"
open Util

let tusenEtt = Seq.initInfinite (fun i->2*i+1) |> Seq.filter isPrime |> Seq.nth 10001

printfn "%d" tusenEtt