open System
open System.Numerics

let rec fac n =
    if (n=BigInteger.One) then
        BigInteger.One
    else
        let a =  n * fac (n-BigInteger.One)
        a
            
let ten = new BigInteger(10)

let sum = 
    let v = fac (new BigInteger(100))
    let rec sum i acc =
        if (i=BigInteger.Zero) then
            acc
        else
            let newAcc = acc + (i % ten)
            let ni = i / ten
            sum ni newAcc 

    sum v BigInteger.Zero

printfn "%s" (sum.ToString())
