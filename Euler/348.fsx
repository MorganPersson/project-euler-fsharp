open System
open System.Collections.Generic
open System.Diagnostics

let stopwatch = Stopwatch()
stopwatch.Start()

let qubes = 
    seq {1L..999L}
    |> Seq.map(fun x -> x, x*x*x)
    |> Array.ofSeq

let squares = 
    seq{1L..30000L}
    |> Seq.map(fun x -> x, x*x)
    |> Array.ofSeq

let findQubeFor p q =
    let rec findSquare min max =
        let n = (max + min) / 2
        let v,sq = squares.[n]
        let sum = sq + q
        match sum=p, max-min <=1, sum>p with
            | true, _, _ -> Some v
            | _, true, true -> None
            | _, true, false -> 
                                let sum =  snd squares.[max] + q
                                if sum=p then Some(fst squares.[max]) else None
            | _, false, true -> findSquare min n
            | _, false, false -> findSquare n max
    findSquare 0 (squares.Length - 1)

let findPalindromes p =
    qubes
    |> Array.map(fun q -> fst q, findQubeFor p (snd q))
    |> Array.filter(fun x -> (snd x).IsSome)
    |> Array.map(fun x -> (snd x).Value, fst x)

let printFound t = 
    let p, lst = t
    printfn "\n%O (%O)" p (stopwatch.Elapsed)
    lst |> Array.iter(fun t ->printfn "\t%O² + %O³" (fst t) (snd t))
    
let palindromes = 
    let rec p x i acc =
        if x > 8000 then acc
        else
            let s1 = x.ToString()
            let s2 = s1.ToCharArray() |> Array.fold(fun s x ->  x.ToString() + s) ""
            if (i=10) then
                let s = Convert.ToInt64(s1+s2)
                p (x+1) 0 (s::acc)
            else 
                let s = Convert.ToInt64(s1 + i.ToString() + s2)
                p x (i+1) (s::acc)
    p 522 0 [] |> Seq.sort

let mutable cnt = 0
let results =
    palindromes
    |> Seq.map(fun p -> p, findPalindromes p)
    |> Seq.filter(fun t -> 
        cnt <- cnt + 1
        if (cnt%1000 = 0) then printf "."
        let foundIt = (snd t).Length = 4
        if foundIt then printFound t
        foundIt)
    |> Seq.take 5
    |> List.ofSeq

let sum = results |> Seq.map fst |> Seq.sum

printfn "The sum of the 5 numbers are %O" sum
stopwatch.Stop()
printfn "That took %O" stopwatch.Elapsed
