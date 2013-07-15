open System
open System.Collections.Generic
open System.Diagnostics

let t = Stopwatch()
t.Start()

let P n = n * (3 * n - 1) / 2

let max = 5000
let pn =
    [0..max]
    |> List.map P

let is_pentagon_num =
    let pnHash = new HashSet<int>(pn)
    pnHash.Contains

let is_pentagon hi low =
    let pn_low = hi - low
    let pn_hi = hi + low
    let pn_low_is_pn = is_pentagon_num pn_low
    let pn_hi_is_pn = is_pentagon_num pn_hi
    pn_low_is_pn && pn_hi_is_pn


let find i =
    let hi = pn.[i]
    let next = pn.[i+1]
    let rec find j =
        let low = pn.[j]
        if j < 1 || next > hi + low then
            None
        else
            let is_pn = is_pentagon hi low
            match is_pn with
                | true  -> Some (i, j, low, hi, (hi - low))
                | false -> find (j-1)
    find (i - 1)

let number = seq{1..max-1} |> Seq.map(fun x -> (find x)) |> Seq.pick(fun x -> x)
let i,j,low,hi,D = number
printfn "The number you are looking for is : %i" D
t.Stop()
printfn "That took %O" t.Elapsed