open System
open System.Collections.Generic
open System.Diagnostics

let t = Stopwatch()
t.Start()

let is_abundant (x:int) =
    let z = Convert.ToSingle(x)
    let upto = int (sqrt z)
    let abundant_sum = 
        seq {1..upto} 
        |> Seq.filter(fun i -> x % i = 0 || i=1) 
        |> Seq.map(fun i -> 
            match i with
                | 1 -> i
                | _ when (x/i <> i) -> i + x/i
                | _ -> i) 
        |> Seq.sum
    abundant_sum > x

let abundant_numbers = 
    [1..28123] 
    |> List.map(fun x -> x, is_abundant x)
    |> List.filter(fun x -> (snd x)) 
    |> List.map fst
    |> Array.ofList

let get_nums x =
    let rec nums min max =
        let n = (max + min) / 2
        let v = abundant_numbers.[n]
        match x < v, max - 1 = min with
            | _, true  -> abundant_numbers.[0..(max-1)]
            | false, _ -> nums n max
            | true , _ -> nums min n
    nums 0 (abundant_numbers.Length - 1)

let ``is not sum of 2 abundant nums`` x =
    let nums = get_nums x
    let rec calc min max i =
        let n = (max + min) / 2
        let k = nums.[n]
        let sum = i + k
        match x = sum, max - 1 <= min, sum<x with
            | true, _, _     -> true
            | false, true, _ -> (i + nums.[max] = x)
            | false, false, true  -> calc n max i
            | false, false, false -> calc min n i
    let max = nums.Length - 1
    let hasValue = nums |> Array.tryFind(fun i -> calc 0 max i) 
    let result = hasValue.IsNone || max = 0
    result

let numbers = 
    [1..28123]
    |> List.filter ``is not sum of 2 abundant nums``
    
let sum = numbers |> Seq.sum

printfn "Sum: %i" sum

t.Stop()
printfn "That took %O" t.Elapsed
