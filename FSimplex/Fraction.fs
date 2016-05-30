module Fraction

let rec GCD (a:int) (b: int) = 
    if b = 0 then a else GCD b a%b


type Frac(n: int,d: int) = 
  member x.n = n 
  member x.d = d 


   
  
  static member reduce (f1: Frac) =
   

  static member inline (+!) (f1: Frac,f2: Frac) = Frac(f1.n+f2.n, f1.d+f2.d)