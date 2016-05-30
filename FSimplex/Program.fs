open Fraction

type Rest = int * int list

let printDic dic = 
  List.iter 
    (fun a -> printf "%d:" (fst a); List.iter (fun e -> printf "%A" e) (snd a); printf "\n")
    dic

let toTableau (d: Frac list list) =
  let slackVars = d.Length
  let augZ = List.init (slackVars-1) (fun _ -> 0)
  let augs = List.map (
                         fun l -> List.mapi (fun i _ -> if i = l then Frac(1,1) else Frac(0,1)) augZ
                       ) [0..slackVars-1]

  let splice l aug =
    snd <| List.fold (fun (i,ac) e -> if i = slackVars - 1 then (i+1,ac@aug@[e]) else (i+1,ac@[e])) (0,[]) l
  List.mapi2 (fun i l a -> (i + slackVars, splice l a)) d augs 

[<EntryPoint>]
let main argv = 
  let input =
     [
       [Frac(1,1);Frac(2,1);Frac(1,1)];
       [Frac(2,1);Frac(2,1);Frac(3,1)];
       [Frac(1,1);Frac(0,1);Frac(0,1)]
     ]
  
 // printDic input
  printfn ""
  printDic <| toTableau input
  
  0 // return an integer exit code
