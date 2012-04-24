open System

// Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
// where each “_” is a single digit.

let expected = "1_2_3_4_5_6_7_8_9_0"
let max = Convert.ToInt64(Math.Sqrt(Convert.ToDouble(1929394959697989990L))) + 1L
let min = (Convert.ToInt64(Math.Sqrt(Convert.ToDouble(1020304050607080900L))) / 10L) * 10L

let asString x = 
    let s = x.ToString()
    let strLen = s.Length
    let rec build idx strAcc = 
        match idx<strLen with
            | false -> strAcc
            | true -> match (idx%2) with
                        | 0 -> build (idx + 1) (strAcc + s.Substring(idx, 1))
                        | _ -> build (idx + 1) (strAcc + "_")
    build 0 ""


let rec find x =
    let y = x*x
    //printfn "%d :: %d" x y
    let s = asString y
    match x >= max with
        | true -> -1L
        | false -> match s with
                    | "1_2_3_4_5_6_7_8_9_0" -> x
                    | _ -> find (x + 10L)

printfn "%d -> %d" min max
let answer = find min
printfn "%d" answer
