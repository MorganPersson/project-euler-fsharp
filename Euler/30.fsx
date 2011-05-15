    open System

    let pow n (exp:int) = int (float n ** float exp)

    let sumOfParts n p =
        let rec sumOfParts n acc =  
            if (n=0) then
                acc
            else
                let reminder = n%10
                let nn = n/10
                sumOfParts nn (reminder::acc)
        let sum = sumOfParts n []
        sum |> List.map (fun f -> pow f p) |> List.sum
    

    let problem p = 
        let max = p*(pow 9 p)
        let numbersToCheck = [for n in [2..max] do yield n]
        let rec problem nums acc =
            match nums with
                | [] -> List.sort acc
                | h::t ->
                            let s = sumOfParts h p
                            if (h=s) then
                                let nAcc = s::acc
                                problem t nAcc
                            else
                                problem t acc

        problem numbersToCheck [] |> List.sum

    let sum = problem 5
    printfn "sum: %d" sum
