open Fraction

type rest = int * int list

let printDic dic = 
  Array.iter 
    (fun a -> Array.iter (fun e -> printf "%A" e) a; printf "\n")
    dic

let toTableau (d: 'a list) =
  let slackVars = d.Length
  let augZ = List.init (slackVars-1) (fun _ -> 0)
  let augs = List.map (
                         fun l -> List.map (fun e -> if e = l then 1 else 0) augZ
                       ) [0..slackVars-1]
  let splice l a = List.fold (fun (i,ac) e -> if i = slackVars - 2 then e::aug@ac) [] l
  List.map2 (fun l a -> splice l a) 

[<EntryPoint>]
let main argv = 
  let input =
     [
       [Frac(1,1);Frac(2,1);Frac(1,1)];
       [Frac(2,1);Frac(2,1);Frac(3,1)];
       [Frac(1,1);Frac(0,1);Frac(0,1)]
     ]
  
  printDic input
  
  0 // return an integer exit code
