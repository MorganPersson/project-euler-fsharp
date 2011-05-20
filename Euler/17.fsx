open System

let digitsEnEnglish = ["Zero";"One";"Two";"Three";"Four";"Five";"Six";"Seven";"Eight";"Nine"]
let specialInEnglish = digitsEnEnglish @ ["Ten";"Eleven";"Twelve";"Thirteen";"Fourteen";"Fifteen";"Sixteen";"Seventeen";"Eighteen";"Nineteen"]
let tensInEnglish = ["Zero";"Ten";"Twenty";"Thirty";"Forty";"Fifty";"Sixty";"Seventy";"Eighty";"Ninety"];

let toEnglish x = 
    let thousands = x / 1000
    let hundreds = (x % 1000) / 100
    let tenish = (x % 20)
    let tens = (x % 100) / 10
    let ones = (x % 10)
    let asString = 
        if (thousands>0) then "onethousand" else ""
        +
        if (hundreds>0) then List.nth digitsEnEnglish hundreds else ""
        +
        if (hundreds>0) then "hundred" else ""
        +
        if (hundreds>0 && (tens>0 || ones > 0)) then "And" else ""
        +
        if (tenish>10 && tens<2) then
            List.nth specialInEnglish tenish
        else
            if (tens>0) then List.nth tensInEnglish tens else ""
            +
            if (ones>0) then List.nth digitsEnEnglish ones else ""
    //printfn "%s" asString
    asString


printfn "  5 = %s" (toEnglish 5)
printfn " 25 = %s" (toEnglish 25)
printfn "125 = %s" (toEnglish 125)
printfn "287 = %s" (toEnglish 287)
printfn "300 = %s" (toEnglish 300)
printfn "1000= %s" (toEnglish 1000)
printfn "415 = %s" (toEnglish 415)
printfn "705 = %s" (toEnglish 705)
printfn "999 = %s" (toEnglish 999)
printfn "100 = %s" (toEnglish 100)

let upTo1000 = [ for n in 1..1000 do yield toEnglish n] |> List.fold (fun f i -> f+i) ""
printfn "\nthousand: %i" (String.length upTo1000)
