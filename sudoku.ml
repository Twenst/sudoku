let grille = [|
  [|9; 0; 0; 1; 0; 0; 0; 0; 5|];
  [|0; 0; 5; 0; 9; 0; 2; 0; 1|];
  [|8; 0; 0; 0; 4; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 8; 0; 0; 0; 0|];
  [|0; 0; 0; 7; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 2; 6; 0; 0; 9|];
  [|2; 0; 0; 3; 0; 0; 0; 0; 6|];
  [|0; 0; 0; 2; 0; 0; 9; 0; 0|];
  [|0; 0; 1; 9; 0; 4; 5; 7; 0|];
  |]
;;

let absentSurLigne k tab i =
  let len = (Array.length tab) -1
  in
  let rec aux tab j =
    match j with
    | x when x = len -> if tab.(i).(j) = k then false 
             else true
    | _ -> if tab.(i).(j) = k then false
           else aux tab (j+1)
  in aux tab 0
;;

let absentSurCol k tab i =
  let present = ref false and j = ref 0 
  in
  while not !present && !j < Array.length tab do
    if tab.(!j).(i) = k then
    present := true;
    incr j;
  done;
  not !present
;;

let absentSurBloc k tab i j =
  let n = int_of_float (sqrt (float_of_int (Array.length tab)))
  in
  let u, v = ref (i -i mod n) ,ref (j -j mod n)
  in
  let present = ref false in

  while not !present && (!u < n || !v < n) do
    
;;
