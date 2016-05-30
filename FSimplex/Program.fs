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

let rec pivot (dic: (int * Frac list) list) step = 
  let l = (snd (dic.Head)).Length
  let a = (snd <| dic.Item(dic.Length-1))
  let (ma,pc,_) = List.fold (fun (max,ind,i) e -> if e > max && i < l-2 then (e,i,i+1) else (max,ind,i+1)) (Frac(0,1),0,0) (snd <| dic.Item(dic.Length-1))
  if(ma.n = 0) then () else
    printfn "Entering: %d" pc
    let (_,line,_) = 
      List.fold 
        (
          fun (min, ind, i) (e: int * Frac list) -> 
            let line = (snd e)
            let pot = line.Item(line.Length-1) / line.Item(pc)
            if pot < min && i < dic.Length-1 then (pot, i, i+1) else (min, ind, i+1)
        ) (Frac(100000,1),-1,0) dic 
  
    printfn "Leaving: %d" (fst (dic.Item(line)))
    let oldLine = dic.Item(line)
    let rat = (snd oldLine).Item(pc)
    let newLine = (pc, List.map (fun f -> f / rat) (snd oldLine)) 
    let dic' = 
      List.mapi 
        (
          fun i (e: int * Frac list) -> 
            if i = line then 
              newLine 
            else
              let elim = (snd e).Item(pc)
              (fst e, List.map2 (fun e el -> e - el * elim) (snd e) (snd newLine))
        ) dic 
    printDic dic'
    printfn "" 
    pivot dic' (step+2)

[<EntryPoint>]
let main argv = 
  let input =
     [
       [Frac(1,1);Frac(2,1);Frac(1,1)];
       [Frac(2,1);Frac(2,1);Frac(1,10)];
       [Frac(0,1);Frac(2,1);Frac(0,1)]
     ]
  
  let tab = toTableau input
 // printDic input
  printfn ""
  printDic tab
  printfn ""
  pivot tab 0
  
  0 // return an integer exit code
