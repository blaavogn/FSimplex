module Fraction

let rec GCD (a:int) (b: int) = 
    if a <> b 
      then 
        if a > b then GCD (a-b) b else GCD a (b-a)
      else a 

type Frac(n: int,d: int) = 
  member x.n = n 
  member x.d = d 
  
  static member reduce (f: Frac) =
    let gcd = GCD f.n f.d
    Frac(f.n/gcd, f.d/gcd)
  
  static member inline (+) (f1: Frac, f2: Frac) = 
    Frac.reduce <| Frac(f1.n * f2.d + f2.n * f1.d, f1.d * f2.d) 
  
  static member inline (*) (f1: Frac, f2: Frac) = 
    Frac.reduce <| Frac(f1.n * f2.n, f1.d * f2.d) 
    
  static member inline (/) (f1: Frac, f2: Frac) = 
    f1 * Frac(f2.d, f2.n)
  
  override x.ToString () =
    if x.d = 1 then 
      sprintf "  %d  " x.n  
    else
      sprintf " %d/%d " x.n x.d