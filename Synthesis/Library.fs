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
       match n >= f with 
       |true ->
         match n % 3 = 0 with 
         |false -> 
           match n % 5 = 0 with
           |false -> factors (n + 1) thr fiv taf
           |true -> factors (n + 1) thr (fiv+1) taf
         |true ->
           match n % 5 = 0 with
           |true -> factors (n + 1) thr fiv (taf+1)
           |false ->factors (n+1) (thr+1) fiv taf
       |false -> thr,fiv,taf
     |true -> 0,0,0
    factors 0 0 0  0
    
let monthDay d y = //I am so sorry
   match d=0 with
   |true -> failwith "Invalid day"
   |false ->
     match isLeap y with
     |false -> 
       match d = 366 with
       |true -> failwith "Invalid day, not leap" 
       |false -> 
          match d <= 31 with
          |true ->"January"
          |false -> 
            match d <=59 with
            |true -> "February"
            |false -> 
             match d <= 90 with
             |true ->"March"
             |false -> 
               match d<=120 with
               |true -> "April"
               |false ->
                 match d<=151 with
                 |true ->"May"
                 |false ->
                  match d<=181 with
                  |true->"June"
                  |false ->
                   match d<=212 with
                   |true ->"July"
                   |false ->
                    match d<= 243 with
                    |true -> "August"
                    |false ->
                     match d <= 273 with
                     |true -> "September"
                     |false ->
                      match d<= 304 with
                      |true -> "October"
                      |false ->
                       match d <= 334 with
                       |true -> "November"
                       |false -> 
                        match d<= 365 with 
                        |true -> "December"
                        |false -> failwith "invalid number"
     |true -> match d <= 31 with
          |true ->"January"
          |false -> 
            match d <=60 with
            |true -> "February"
            |false -> 
             match d <= 91 with
             |true ->"March"
             |false -> 
               match d<=121 with
               |true -> "April"
               |false ->
                 match d<=152 with
                 |true ->"May"
                 |false ->
                  match d<=182 with
                  |true->"June"
                  |false ->
                   match d<=213 with
                   |true ->"July"
                   |false ->
                    match d<= 244 with
                    |true -> "August"
                    |false ->
                     match d <= 274 with
                     |true -> "September"
                     |false ->
                      match d<= 305 with
                      |true -> "October"
                      |false ->
                       match d <= 335 with
                       |true -> "November"
                       |false -> 
                        match d<= 366 with 
                        |true -> "December"
                        |false -> failwith "invalid number"

let coord _ =
    failwith "Not implemented"