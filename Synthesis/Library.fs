module Synthesis

let abelar c =
    c > 12 && c < 3097 && c % 12 =0 

let area b h =
    match b>=0.0 , h>=0.0 with
    |false, _ | _ , false -> failwith "Negative values" 
    |true, true -> (0.5*b)*h
      


let zollo c =
    match c>0 with
    |true -> c*2
    |false -> c*(-1)

let min a b =
    match a>=b with
    |true -> b
    |false -> a

let max a b =
    match a>=b with
    |true -> a
    |false -> b

let ofTime h m s =
    s + (m*60) + (h*3600)

let toTime t =
    match t>=0 with
    |false -> 0,0,0
    |true -> let h = t/3600
             let m =(t-(h*3600))/60
             let s = t-(h*3600)-(m*60)
             h,m,s

let digits d =
    let rec count c acc=
     match c<=9 && c>=(-9) with
     |true -> acc
     |false -> count (c/10) (acc+1)
    count d 1

    
let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"