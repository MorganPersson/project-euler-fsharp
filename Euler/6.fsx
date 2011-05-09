let square i = i*i
let sumOfSquares = [1..100] |> Seq.map square |> Seq.sum
let squareOfSum = [1..100] |> Seq.sum |>  square

let diff = squareOfSum - sumOfSquares
printfn "%d - %d = %d" squareOfSum sumOfSquares diff
