open System.Numerics

let calc (n:int) = 
    let b = new BigInteger(n)
    b**n


let problem n = 
    let sum = [for i in [1..n] do yield calc i]
                |> List.sum
    let lastTenDigits = sum % (new BigInteger(10000000000L))
    lastTenDigits
   
let result = problem 1000
printfn "%s" (result.ToString())
