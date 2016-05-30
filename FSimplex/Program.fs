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
    snd <| List.fold (fun (i,ac) e -> if i = slackVars - 1 then (i+1,ac@aug@[e]) else (i+1,ac@[e])) (0,[]) l //adr
  List.mapi2 (fun i l a -> (i + slackVars, splice l a)) d augs 

let pivot (dic: (int * Frac list) list) step = 
  let l = (snd (dic.Head)).Length
  let a = (snd <| dic.Item(dic.Length-1))
  let pc = snd <| List.fold (fun (max,i) e -> if e > max && i < l then (e,i+1) else (max, i+1)) (Frac(0,1),0) (snd <| dic.Item(dic.Length-1))
  printf "Entering %d" pc
  ()

[<EntryPoint>]
let main argv = 
  let input =
     [
       [Frac(1,1);Frac(2,1);Frac(1,1)];
       [Frac(2,1);Frac(2,1);Frac(3,1)];
       [Frac(1,1);Frac(0,1);Frac(0,1)]
     ]
  
  let tab = toTableau input
 // printDic input
  printfn ""
  printDic tab
  printfn ""
  pivot tab 0
  
  0 // return an integer exit code
