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

let grilleVide = [|
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  |]
;;

let absentSurLigne k tab i =
  let len = (Array.length tab)
  in
  let rec aux tab j =
    match j with
    | x when x = len - 1-> if tab.(i).(j) = k then false 
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

let absentSurBloc k g l c =
  let n = int_of_float(sqrt(float_of_int(Array.length g))) in
  let u = l-(l mod n) and v = c-(c mod n) in (* l ligne du premier élément, c colonne du premier élément *)
  try
    for u'=u to u+n-1 do
      for v'=v to v+n-1 do
        if g.(u').(v') = k then raise Exit
      done;
    done;
    true;
  with Exit -> false
;;

let afficheGrille tab =
  let nbl = Array.length tab
  in
  let n = int_of_float (sqrt (float_of_int nbl))
  in
  let printLineSep nbl =
    print_string "\n";
    for i = 1 to n do
      for j = 1 to n do
        print_string "--"
      done;
      if n*i <> nbl then print_string "|-"
    done;
    print_string "\n"
  in
  print_string "\n";
  let rec aux x y =
    match x, y with
    | x, y when x=nbl-1 && y=nbl-1 -> print_int tab.(y).(x); print_string "\n"
    | x, y when x=nbl-1 && y mod n =n-1 -> print_int tab.(y).(x); printLineSep nbl; aux 0 (y+1) 
    | x, _ when x=nbl-1 -> print_int tab.(y).(x); print_string "\n"; aux 0 (y+1)
    | x, _ when x mod n =n-1 -> print_int tab.(y).(x); (print_string " | "); aux (x+1) y
    | x, _ -> print_int tab.(y).(x); (print_string " ");  aux (x+1) y
  in aux 0 0
;;
afficheGrille grille;;

exception Trouve;;  
let remplir tab =
  let nbl = Array.length tab in
  let rec aux p =
    if p = nbl*nbl then true
    else
      begin
        let j = p/nbl and i = p mod nbl in
        if tab.(i).(j) <> 0 then aux (p+1) (* case non vide = on passe à la case d'après*)
        else
          try
            for k = 1 to nbl do
              if (absentSurBloc k tab i j && absentSurLigne k tab i && absentSurCol k tab j) then
              begin
                tab.(i).(j) <- k;
                if aux(p+1) then raise Trouve
              end
            done;
            tab.(i).(j) <- 0; false
          with Trouve -> true
      end
  in aux 0
;;

remplir grille;;
afficheGrille grille;;
