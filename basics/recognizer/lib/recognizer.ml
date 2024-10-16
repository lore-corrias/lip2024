let rec lang1 w = match w with 
  ['0'] -> true |
  ['1'] -> true |
  '0' :: w -> lang1 w |
  '1' :: w -> lang1 w |
  _ -> false;;

let lang2 w = 
  let rec ones w = match w with
    [] -> true |
    '1' :: w -> ones w |
    _ -> false
    in 
  match w with
  [] -> true |
  ['0'] -> true |
  ['1'] -> true |
  '0' :: w -> ones w |
  '1' :: w -> ones w |
  _ -> false;;

let lang3 w = 
  let rec zero_ones w = match w with
    ['0'] -> true |
    ['1'] -> false |
    '0' :: w -> zero_ones w |
    '1' :: w -> zero_ones w |
    _ -> false
    in 
  match w with
  ['0'] -> true |
  ['1'] -> false |
  '0' :: w -> zero_ones w |
  _ -> false;;

let rec lang4 w = 
  let rec state3 w = match w with
    [] -> true |
    ['0'] -> true |
    '0' :: w -> state3 w |
    _ -> false
    in
  let rec state2 w = match w with
    [] -> false |
    ['1'] -> true |
    '0' :: w -> state2 w |
    '1' :: w -> state3 w |
    _ -> false
    in
  match w with
   [] -> false |
   '0' :: w -> lang4 w |
   '1' :: w -> state2 w |
   _ -> false

let rec lang5 w = 
  let state3 w = match w with
    ['1'] -> true |
    '1' :: w -> state1 w |
    _ -> false
  in
  let state1 w = match w with
    [] -> true |
    '0' :: w -> state2 w |
    '1' :: w -> state3 w |
    _ -> false
  in
    let state2 w = match w with
    ['0'] -> true |
    '0' :: w -> state1 w |
    _ -> false
  in
    match w with
    [] -> false |
    w -> state1 w
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
