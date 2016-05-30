open Fraction


[<EntryPoint>]
let main argv = 
  let f1 = Frac(3,7)
  let f2 = Frac(4,7)
    
  let res = f1 + f2

  printfn "%s" <| res.ToString ()
  0 // return an integer exit code
