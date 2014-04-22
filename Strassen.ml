let do_operation m1 m2 operation = 
    let row = Array.length m1 in
    let col = Array.length m1.(0) in 
    if row = Array.length m2 && col = Array.length m2.(0) then
        (let result = zero row col in
        for i = 0 to row - 1 do
            for j = 0 to col - 1 do
                result.(i).(j) <- operation m1.(i).(j) m2.(i).(j)
            done;
        done;
        result)
    else raise IncompatibleDimensions;;

let add m1 m2 =
        do_operation m1 m2 M.add;;

let sub m1 m2 = 
        do_operation m1 m2 M.sub;;

    (* TODO *)
    (* Check if they can multiply *)
    (* If they can, pad them if necessary *)
    (* Remove padding using split *)

let mul m1 m2 =
    mul_invariant m1 m2;; 

let rec mul_invariant m1 m2 =
    let row = Array.length m1 in
    let result = zero row row in
    if row = 1 then 
        (result.(0).(0) = M.mul m1.(0).(0) m2.(0).(0); result)
    else
        let dim = row / 2 in

        (* Create halves *)

        let a11 = zero dim dim in
        let a12 = zero dim dim in
        let a21 = zero dim dim in
        let a22 = zero dim dim in
        let b11 = zero dim dim in
        let b12 = zero dim dim in
        let b21 = zero dim dim in
        let b22 = zero dim dim in

        (* Split m1 *)
        split m1 a11 0 0; 
        split m1 a12 0 dim; 
        split m1 a21 dim 0; 
        split m1 a22 dim dim; 

        (* Split m2 *)

        split m2 b11 0 0; 
        split m2 b12 0 dim; 
        split m2 b21 dim 0; 
        split m2 b22 dim dim; 

        (*
              M1 = (a11 + a22)(b11 + b22)
              M2 = (a21 + a22) b11
              M3 = a11 (b12 - b22)
              M4 = a22 (b21 - b11)
              M5 = (a11 + a12) b22
              M6 = (a21 - a11) (b11 + b12)
              M7 = (a12 - a22) (b21 + b22)
        *)

        let M1 = mul_invariant (add a11 a22) (add b11 b22) in 
        let M2 = mul_invariant (add a21 a22) b11 in 
        let M3 = mul_invariant a11 (sub b12 b22) in
        let M4 = mul_invariant a22 (sub b21 b11) in
        let M5 = mul_invariant (add a11 a12) b22 in
        let M6 = mul_invariant (add a21 a11) (add b11 b12) in
        let M7 = mul_invariant (sub a12 a22) (add b21 b22) in

        (*
          C11 = M1 + M4 - M5 + M7
          C12 = M3 + M5
          C21 = M2 + M4
          C22 = M1 - M2 + M3 + M6
        *) 

        let C11 = add (sub (add M1 M4) M5) M7 in
        let C12 = add M3 M5 in
        let C21 = add M2 M4 in
        let C22 = add (sub (add M1 M3) M2) M6 in 

        join C11 result 0 0; 
        join C12 result 0 dim; 
        join C21 result dim 0; 
        join C22 result dim dim;

        result;;


let split parent child row col = 
    let last = Array.length child - 1 in
    for i = 0 to last do
        for j = 0 to last do
            child.(i).(j) <- parent.(i + row).(j + col)
        done;
    done;;

let join parent child row col = 
    let last = Array.length child - 1 in
    for i = 0 to last do
        for j = 0 to last do
            parent.(i + row).(j + col) <- child.(i).(j) 
        done;
    done;; 