open System

let hasNode x y lst = 
    List.exists(fun f ->
                let x1, y1, n = f
                (x1=x && y1=y)) lst

let getNode x y lst = 
    List.find(fun f ->
                let x1, y1, n = f
                (x1=x && y1=y)) lst
    
let route width = 
    let rec route x y visited = 
        if (x=width || y = width) then
            (x,y,1L) :: visited
        else
            if hasNode x y visited then
                visited
            else
                let v1 = route   x   (y+1) visited
                let v2 = route (x+1)  y    v1
                let _,_,n1 = getNode x (y+1) v2
                let _,_,n2 = getNode (x+1) y v2
                (x,y,(n1+n2))::v2
                                
    route 0 0 []

let routes = route 20
let theRoute = getNode 0 0  routes
let _,_,n = theRoute
printfn "routes: %d" n
             