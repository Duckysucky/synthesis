module Synthesis

open System.Diagnostics
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
    match min a b < min c d, max a b > max c d with
    |true, false -> min a b, max c d
    |false, true -> min c d, max a b
    |true, true -> min a b, max a b
    |false, false -> min c d, max c d

let isLeap y =
   match y >= 1582 with
   |false -> failwith "Invalid year"
   |true -> match y % 4 = 0, y % 100 = 0, y % 400 = 0  with 
   |true,true,true | true, false, false -> true
   |_ -> false


let month  = function
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


let toBinary n =
    failwith "Not implemented"
    (*match n > 0 with
    |false -> failwith "Invalid value"
    |true -> 
    let rec bin a b= match a >=1 with
    |false -> bin (a/2) ((sprintf a%2 i) + b)
    |true -> sprintf a + b 
    bin n ""
    *)
    

let bizFuzz f =
    let rec factors n thr fiv taf =
     match f <=1 with
     |false ->
       match n <= f with 
       |true ->
         match n % 3 = 0 with 
         |false -> 
           match n % 5 = 0 with
           |false -> factors (n + 1) thr fiv taf
           |true -> factors (n + 1) thr (fiv+1) taf
         |true ->
           match n % 5 = 0 with
           |true -> factors (n + 1) (thr+1) (fiv+1) (taf+1)
           |false ->factors (n+1) (thr+1) fiv taf
       |false -> thr,fiv,taf
     |true -> 0,0,0
    factors 1 0 0 0
    
let monthDay d y = 
  match isLeap y, d>365 with
  |false,true -> failwith "Invalid function"
  |_ ->
   match d <= 0 || d > 366 with
   |true -> failwith "Invalid day"
   |false -> 
      let rec id a acc =
       match a >= d with
       |true -> 
        let h,p = month acc
        h
       |false ->
       match acc = 1 || acc = 3 || acc = 5 || acc = 7 || acc = 8 || acc = 10 with
       |true -> id (a + 31) (acc + 1)
       |false ->  
          match acc = 2, isLeap y with
          |true, false -> id (a+28) (acc+1)
          |true, true -> id(a+29) (acc+1)
          |_ -> id(a + 30) (acc+1)
      id 1 1
      
     

let coord _ =
    failwith "Not implemented"