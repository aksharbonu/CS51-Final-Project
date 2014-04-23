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

        let scalar value m1 =
        let row = Array.length m1 in
        let col = Array.length m1.(0) in 
        let result = zero row col in
            for i = 0 to row - 1 do
                for j = 0 to col - 1 do
                    result.(i).(j) <- M.mul m1.(i).(j) value
                done;
            done;
            result
        end

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

let fill result child = 
    for i = 0 to Array.length child - 1 do
        for j = 0 to Array.length m1.(0) - 1 do
            result.(i).(j) <- child.(i).(j)
        done;
    done; result;; 

let pad m1 = 
    if row1 = col1 && row1 = 1 then m1
    else 
    (let row1 = Array.length m1 in
    let col1 = Array.length m1.(0) in
    match row1 mod 2 = 0, col1 mod 2 = 0 with
    | true, true -> m1
    | true, false -> fill (zero (row1 + 1) col1) m1
    | false, true -> fill (zero row1 (col1 + 1)) m1
    | _, _ ->  fill (zero (row1 + 1) (col1 + 1)) m1)

let remove_pad result result_padded =
    for i = 0 to Array.length result - 1 do
        for j = 0 to Array.length result.(0) - 1 do
            result.(i).(j) <- result_padded.(i).(j)
        done;
    done;

let split parent child row col = 
    for i = 0 to Array.length child - 1 do
        for j = 0 to Array.length child.(0) - 1 do
            child.(i).(j) <- parent.(i + row).(j + col)
        done;
    done;;

let join parent child row col = 
    for i = 0 to Array.length child - 1 do
        for j = 0 to Array.length child.(0) - 1 do
            parent.(i + row).(j + col) <- child.(i).(j) 
        done;
    done;;

let rec mul_invariant matrix1 matrix2 =
    let row = Array.length matrix1 in
    let result = zero row row in
    if row = 1 then 
        (result.(0).(0) <- M.mul matrix1.(0).(0) matrix2.(0).(0); result)
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

        (* Split matrix 1 *)
        split matrix1 a11 0 0; 
        split matrix1 a12 0 dim; 
        split matrix1 a21 dim 0; 
        split matrix1 a22 dim dim; 

        (* Split m2 *)

        split matrix2 b11 0 0; 
        split matrix2 b12 0 dim; 
        split matrix2 b21 dim 0; 
        split matrix2 b22 dim dim; 

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

        join result c11 0 0; 
        join result c12 0 dim; 
        join result c21 dim 0; 
        join result c22 dim dim;

        result 

    and

    (* Remove & add padding *)
    mul_pad m1 m2 =
        let m1_padded = pad m1 in
        let m2_padded = pad m2 in
        let result_padded = mul_invariant m1_padded m2_padded in
        let result = zero (Array.length m1) (Array.length m2.(0))
        remove_pad result result_padded; result;;

    (* Check if multiplication can be done *)
    mul m1 m2 =
        let row = Array.length m1 in
        let col = Array.length m1.(0) in
        if col = Array.length m2 then mul_pad m1 m2
        else raise IncompatibleDimensions