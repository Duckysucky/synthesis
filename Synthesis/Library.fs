module Synthesis

open System.Diagnostics

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

    
let minmax (a,b,c,d) =
    failwith "Not implemented"

let isLeap y =
   match y >= 1582 with
   |false -> failwith "Invalid year"
   |true -> match y % 4 = 0, y % 100 = 0, y % 400 = 0  with 
   |true,true,true | true, false, false -> true
   |_ -> false


let month m = 
    match m with 
    |1-> "January",31
    |2 ->"February", 28
    |3 -> "March", 31
    |4 ->"April", 30
    |5 ->"May", 31
    |6 ->"June", 30
    |7 ->"July", 31
    |8 ->"August", 31
    |9 ->"September", 30
    |10 ->"October", 31
    |11 ->"November", 30
    |12 ->"December", 31
    |_ -> failwith "Invalid Function"


let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"