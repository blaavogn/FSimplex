﻿module Fraction

let rec GCD (a:int) (b: int) = 
    if a <> b 
      then 
        if a > b then GCD (a-b) b else GCD a (b-a)
      else a 

type Frac(n: int,d: int) = 
  member x.n = n 
  member x.d = d 
  
  static member reduce (f: Frac) =
    
    //let gcd = if f.n = 0 then 1 else GCD f.n f.d
    Frac(f.n/1, f.d/1)
  
  static member inline (+) (f1: Frac, f2: Frac) = 
    Frac.reduce <| Frac(f1.n * f2.d + f2.n * f1.d, f1.d * f2.d) 
  
  static member inline (-) (f1: Frac, f2: Frac) = 
    Frac.reduce <| Frac(f1.n * f2.d - f2.n * f1.d, f1.d * f2.d) 
  
  static member inline (+) (f1: Frac, i: int) = 
    Frac.reduce <| Frac(f1.n + i * f1.d, f1.d) 
  
  static member inline (*) (f1: Frac, f2: Frac) = 
    Frac.reduce <| Frac(f1.n * f2.n, f1.d * f2.d) 
    
  static member inline (/) (f1: Frac, f2: Frac) = 
    f1 * Frac(f2.d, f2.n)
  
  override x.Equals (oth: obj) =
    match oth with
        | :? Frac as f2 -> 
            let f1' = Frac.reduce x
            let f2' = Frac.reduce f2
            f1'.n = f2'.n && f1'.d = f2'.d
        | _ -> false 

  interface System.IComparable with 
    member x.CompareTo oth = 
      match oth with
        | :? Frac as f2 -> 
            let th = x.n * f2.d
            let ot = x.d * f2.n
            if th < ot then -1 elif th = ot then 0 else 1
        | _ -> failwith "Illegal comparison" 

  override x.ToString () =
    if x.d = 1 then 
      sprintf "  %2d  " x.n  
    else
      sprintf " %d/%d " x.n x.d