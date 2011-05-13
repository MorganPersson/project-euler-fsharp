open System.Numerics
open System

let ten = new BigInteger(10)

let sum v = 
    let rec sum i acc =
        if (i=BigInteger.Zero) then
            acc
        else
            let newAcc = acc + (i % ten)
            let ni = i / ten
            sum ni newAcc 

    sum v BigInteger.Zero

let two = new BigInteger(2)
let f = sum (two**1000)
printfn "%s" (f.ToString())

