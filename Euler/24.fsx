open System

let switch list =
    let h = List.head list
    let t = list |> Seq.skip 1 |> Seq.head
    [[h;t];[t;h]]

let listWithoutNthItem list n = 
    let i = Seq.nth n list
    let l = list |> Seq.filter (fun f-> f <> i) |> List.ofSeq
    (i, l)

let permute list =
    let rec permute list =
        if (List.length list = 2) then 
            switch list
        else
            let result = 
                [ for i in [0..(List.length list - 1)] do 
                    let h,t = listWithoutNthItem list i
                    let p = permute t
                    let res = List.map (fun f-> h::f) p
                    for l in res do yield l
                ]
            result 
    permute list       
// ------------------------------

let lst = [0;1;2;3;4;5;6;7;8;9]
let permutes = permute lst
let millionth = List.nth permutes 999999 
List.iter (fun n -> printf "%d" n) millionth
   