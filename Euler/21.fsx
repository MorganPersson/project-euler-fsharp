open System

let findNumsDivisibleWith n = 
    [for i in [1..(n-1)] do if (n%i)=0 then yield i]

let sumOfProperDivisors n = 
    Seq.sum n


let divs220 = findNumsDivisibleWith 220
let sumDivs200 = sumOfProperDivisors divs220

let isAmicable n lst = 
    let n1, n2 = n
    let l1,l2 = List.nth n2 lst
    if ((n1 = l2) && (n1 = l2)) then true else false

let amicableNumbers n = [for i in [2..n] do
                            let iSum =  sumOfProperDivisors (findNumsDivisibleWith i)
                            let bSum = sumOfProperDivisors (findNumsDivisibleWith iSum)
                            if (bSum = i && i<>iSum) then 
                                printfn "%d" i
                                yield i]

let sumOfAmicable = amicableNumbers 10000
                    |> List.sum

printfn "sum of amicable numbers = %d" sumOfAmicable
