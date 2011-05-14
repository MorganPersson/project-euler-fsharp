open System.Numerics

let stop = BigInteger.Pow(new BigInteger(10),999)
let fib = 
    let rec fib a b term = 
        let n = a + b
        if (n>=stop)
            then term
        else
            fib b n (term+1) 
    fib (BigInteger.One) (BigInteger.One) 3

printfn "%d" (stop.ToString().Length)
printfn "%s" (fib.ToString())
