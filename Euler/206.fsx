open System

// Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
// where each “_” is a single digit.

let expected = "1_2_3_4_5_6_7_8_9_0"
let max = Convert.ToInt64(Math.Sqrt(Convert.ToDouble(1929394959697989990L))) + 1L
let min = (Convert.ToInt64(Math.Sqrt(Convert.ToDouble(1020304050607080900L))) / 10L) * 10L

let isCorrect x =
    let rec check value num = 
        match value with
        | 0L when num = 0L -> true
        | 0L when num > 0L -> false
        | _ -> let m = value%10L
               match m - num with
               | 0L -> check (value / 100L) (num-1L)
               | _ -> false
    check (x / 100L) 9L


let rec find x =
    let y = (x*x)
    let s = isCorrect y
    match s with
        | true -> x
        | _ when (x < max) -> find (x + 10L)
        | _ -> -1L

printfn "%d -> %d" min max
let answer = find min
printfn "%d" answer
