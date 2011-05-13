open System.IO
open System

let fileName = @"C:\code\F#\Euler\Euler\names.txt"

let loadFile f =
    let p = Path.GetFullPath(fileName) 
    let text = File.ReadAllText(fileName)
    let txt = text.Replace("\"", "")
    txt.Split ([|','|])
    |> List.ofArray

let toBytes s = 
    System.Text.Encoding.ASCII.GetBytes(s.ToString())  |> List.ofArray
    
let nameValue s = 
    toBytes s 
        |> List.map (fun b -> (int)b - 64) 
        |> List.sum

let result = 
    loadFile fileName
            |> List.sort 
            |> List.mapi (fun i s -> (i+1) * (nameValue s)) 
            |> List.sum

printfn "%d" result