let numToList n = 
    let rec num2List n lst =
        let remainder = n%10
        let d = n/10
        let newAcc = remainder::lst
        if (d=0) then
            List.sort newAcc
        else
            num2List d newAcc
    num2List n []

let findSmallest = 
    let rec findSmallestX n =
        let once  = numToList (1*n)
        let two   = numToList (2*n)
        let three = lazy numToList (3*n)
        let four  = lazy numToList (4*n)
        let five  = lazy numToList (5*n)
        let six   = lazy numToList (6*n)
        if (once = two && once=three.Value && once=four.Value && once=five.Value && once=six.Value) then
            n
        else
            findSmallestX (n + 1)
    findSmallestX 1

let n = findSmallest
printfn "%d" n