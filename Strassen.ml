exception IncompatibleDimensions

        type elt = M.t

        type t = elt array array 

        let of_array = ident;;

        let to_array = ident;;

        let zero n m = Array.make_matrix n m M.zero;;

        let identity n =
            let result = zero n n in
            for i = 0 to n - 1 do
                result.(i).(i) <- M.one
            done;
            result;;


        let dim m1 = Array.length m1, Array.length m1.(0);; 

        let scalar value m1 =
            let row, col = dim m1 in
            let result = zero row col in
                for i = 0 to row - 1 do
                    for j = 0 to col - 1 do
                        result.(i).(j) <- M.mul m1.(i).(j) value
                    done;
                done;
            result;;


let do_operation m1 m2 operation = 
    let row, col = dim m1 in
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

(* Adds the child matrix to the parent matrix starting at index (row, col) *)
let join parent child row col =  
    let row, col = dim child in
    for i = 0 to row - 1 do
        for j = 0 to col - 1 do
            parent.(i + row).(j + col) <- child.(i).(j) 
        done;
     done; parent;;

(* Checks if dimensions are both 1 (base case) or odd dimensions and makes even *)
let pad m1 = 
    let row1, col1 = dim m1 in
    if row1 = col1 && row1 = 1 then m1
    else 
    (match row1 mod 2 = 0, col1 mod 2 = 0 with
    | true, true -> m1
    | true, false -> join (zero row1 (col1 + 1)) m1 0 0 
    | false, true -> join (zero (row1 + 1) col1) m1 0 0
    | _, _ ->  join (zero (row1 + 1) (col1 + 1)) m1 0 0);;

(* Adds the parent matrix to the child matrix starting at index (row, col) till child is full *)
let split parent child row col =
    let row, col = dim child in 
    for i = 0 to row - 1 do
        for j = 0 to col - 1 do
            child.(i).(j) <- parent.(i + row).(j + col)
        done;
     done; child;;

let rec mul_invariant matrix1 matrix2 =
    (* Saves rows & columns of matrices for future use*)
    let row1, col1 = dim matrix1 in
    let row2, col2 = dim matrix2 in
    let result = zero row1 row1 in
    if row1 = 1 then 
        (result.(0).(0) <- M.mul matrix1.(0).(0) matrix2.(0).(0); result)
    else
        let half_row1 = row1 / 2 in
        let half_col1 = col1 / 2 in
        let half_row2 = row2 / 2 in
        let half_col2 = col2 / 2 in

        (* Create halves *)

        let a11 = zero half_row1 half_col1 in
        let a12 = zero half_row1 half_col1 in
        let a21 = zero half_row1 half_col1 in
        let a22 = zero half_row1 half_col1 in
        let b11 = zero half_row2 half_col2 in
        let b12 = zero half_row2 half_col2 in
        let b21 = zero half_row2 half_col2 in
        let b22 = zero half_row2 half_col2 in

        (* Split matrix 1 *)
        let _ = split matrix1 a11 0 0 in 
        let _ = split matrix1 a12 0 half_col1 in 
        let _ = split matrix1 a21 half_row1 0 in 
        let _ = split matrix1 a22 half_row1 half_col1 in

        (* Split m2 *)

        let _ = split matrix2 b11 0 0 in 
        let _ = split matrix2 b12 0 half_col2 in 
        let _ = split matrix2 b21 half_row2 0 in 
        let _ = split matrix2 b22 half_row2 half_col2 in 

        (*
              M1 = (a11 + a22)(b11 + b22)
              M2 = (a21 + a22) b11
              M3 = a11 (b12 - b22)
              M4 = a22 (b21 - b11)
              M5 = (a11 + a12) b22
              M6 = (a21 - a11) (b11 + b12)
              M7 = (a12 - a22) (b21 + b22)
        *)

        let m1 = mul_pad (add a11 a22) (add b11 b22) in 
        let m2 = mul_pad (add a21 a22) b11 in 
        let m3 = mul_pad a11 (sub b12 b22) in
        let m4 = mul_pad a22 (sub b21 b11) in
        let m5 = mul_pad (add a11 a12) b22 in
        let m6 = mul_pad (sub a21 a11) (add b11 b12) in
        let m7 = mul_pad (sub a12 a22) (add b21 b22) in

        (*
          C11 = M1 + M4 - M5 + M7
          C12 = M3 + M5
          C21 = M2 + M4
          C22 = M1 - M2 + M3 + M6
        *) 

        let c11 = add (sub (add m1 m4) m5) m7 in
        let c12 = add m3 m5 in
        let c21 = add m2 m4 in
        let c22 = add (sub (add m1 m3) m2) m6 in 

        (* Adds the elements of the 2nd matrix to the first *)
        let _ = join result c11 0 0 in  
        let _ = join result c12 0 half_row1 in  
        let _ = join result c21 half_row1 0 in  
        let _ = join result c22 half_row1 half_row1 in
        result 

    and

    (* Remove & add padding *)
    mul_pad m1 m2 =
        let m1_padded = pad m1 in
        let m2_padded = pad m2 in
        let result_padded = mul_invariant m1_padded m2_padded in
        if dim m1 = dim m1_padded && dim m2 = dim m2_padded then result padded
        else let result = zero (Array.length m1) (Array.length m2.(0)) in
        let _ = split result_padded result 0 0 in
        result;;

    (* Check if multiplication can be done *)
    let mul m1 m2 =
        if Array.length m1.(0) = Array.length m2 then mul_pad m1 m2
        else raise IncompatibleDimensions