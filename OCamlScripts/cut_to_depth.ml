type beta = 
  | Top 
  | Bot
  | Name of string

type bop = 
  | Leq
  | Geq
  | Eq

type tau = {b : beta; ref : r}
and 
delta = {tag : string; bo : bop; t : tau}
and 
r = delta list

let cut_ref k r =
  (let rec cut_ref_helper k r cur_depth =
    if cur_depth >= k then
      []
    else 
      let map_f x = 
        match x with 
        |{tag ;bo; t = {b;ref}} -> 
          if List.length ref = 0 then 
            x
          else 
            {tag = tag; bo = bo; t = {b = b; 
                                  ref = (cut_ref_helper k ref (cur_depth + 1))}}
          in
      List.map map_f r in
  cut_ref_helper k r 0)

(*{t <= A}*)
let refinement1 = [{tag = "t"; bo = Leq; t = {b = Name "A"; ref = []}}]

(*{t <= A {t <= A }}*)
let refinement2 = [{tag = "t"; bo = Leq; t = {b = Name "A"; ref = 
  [{tag = "t"; bo = Leq; t = {b = Name "A"; ref = 
  []}}]}}]
let x = cut_ref 1 refinement1
let y = cut_ref 1 refinement2


  
 

  
  
