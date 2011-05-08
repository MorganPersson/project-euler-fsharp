let num = 600851475143L

let findLargestPrime n =
    let rec largestPrime n d ds =
        match n with
        | 1L -> Seq.max ds
        | x -> if (n%d)=0L then (largestPrime (n/d) 2L (d::ds)) else largestPrime n (d+1L) ds
    largestPrime n 2L [1L]

printfn "%d" (findLargestPrime num) 