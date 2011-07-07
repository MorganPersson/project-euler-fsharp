#load "util.fsx"
open System

let rec rotate (number:string) nTimes = 
    if (nTimes=0) then
        number
    else
        let n = number.ToCharArray() |> Array.toList
        let nList = List.tail n @ [List.head n] //not so efficient
        let s = List.fold (fun acc n -> acc + n.ToString()) "" nList
        rotate s (nTimes-1) 
            
let isCircularPrime p primes =
    let numbersToProduce = String.length p
    let numbers = [for n in 1..numbersToProduce do yield rotate p (n-1)]
    let circularsFound = List.filter (fun f -> List.exists (fun g -> g=f) primes) numbers
    circularsFound.Length = numbersToProduce


let primes = Util.primes 999999 |> List.map (fun f -> f.ToString())

let circularPrimes = List.filter (fun f -> isCircularPrime f primes) primes

printfn "Number of circular primes: %i" (List.length circularPrimes)
List.iter (fun i -> printf "%s, " i) circularPrimes
