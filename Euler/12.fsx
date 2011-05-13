open System

let nextTriangleNumber n = 
    let i = fst n + 1
    let v = snd n
    (i, i + v)
    
let triangleNumbers i = 
    [1..i] |> List.mapi (fun i f -> ((i+1), f + (i+1)))

let factors i = 
    let max = (int (sqrt(float i))) + 1
    let pairsToBuild = [2..max]
    [   yield 1 
        yield i
        for n in [2..max] do
        if (i%n)=0 then 
            yield n
            yield i/n]


let five01 =
    let rec repeat n =
        let i = snd n
        let nums = factors i
        if (Seq.length nums > 500) then
            i
        else
            repeat (nextTriangleNumber n)
    repeat (1,1)

printfn "%d" (five01)
