open Core.Std
open Ring

(* Define a generic matrix type and its operations *)
module type MATRIX =
sig
    exception IncompatibleDimensions
    type elt
    type t
    val of_array: elt array array -> t
    val to_array: t -> elt array array
    val zero : dimx: int -> dimy: int -> t
    val identity : int -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val scalar : elt -> t -> t
    val det : t -> elt
    val mul : t -> t -> t
    val solve : t -> t -> t * t
    val lu_decomp : t -> t * t * t
end

(* A functor that takes in a RING that specifies the type of element in the matrix, and outputs a matrix module *)
module MatrixFunctor (M : RING) : MATRIX with type elt = M.t =
    struct

        exception IncompatibleDimensions

        type elt = M.t

        (* Use 2-dimensional arrays to represent matrices - easy to access indices *)
        type t = elt array array 

        let dim m1 = Array.length m1, Array.length m1.(0);; 

        (* 
            Checks to make sure basic invariant is followed:
            - In every row, same number of elements

            The following two methods allow us to convert between the abstract type and arrays
        *)

        let of_array m = 
            let col = Array.length m.(0) in 
            if Array.for_all m ~f:(fun row -> col = Array.length row) then ident m
            else raise IncompatibleDimensions;;

        let to_array = ident;;

        (* Functions to create generic matrices - zero and identity *)
        let zero ~dimx:n ~dimy:m = Array.make_matrix ~dimx:n ~dimy:m M.zero;;

        let identity n =
            let result = zero ~dimx:n ~dimy:n in
            for i = 0 to n - 1 do
                result.(i).(i) <- M.one
            done;
            result;;

        (* Multiply every element in a matrix by a scalar *)
        let scalar value m1 =
            let row, col = dim m1 in
            let result = zero ~dimx:row ~dimy:col in
                for i = 0 to row - 1 do
                    for j = 0 to col - 1 do
                        result.(i).(j) <- M.mul m1.(i).(j) value
                    done;
                done;
            result;;

        (* 
            Do a certain operation to every corresponding element of two matrices.
            Both 'add' and 'sub' below use this method.
         *)
        let do_operation m1 m2 operation = 
            let row, col = dim m1 in
            if (row, col) = dim m2 then
                (let result = zero ~dimx:row ~dimy:col in
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

        (* A method that swaps values in two arrays from one index to another *)
        let swap_array_from_to a1 a2 first last =
            for i = first to last do
                let temp_val = a1.(i) in
                a1.(i) <- a2.(i);
                a2.(i) <- temp_val;
            done;
        ;;

        (* Computes the determinant of a (square) matrix *)
        let rec det m =
            let row, col = dim m in
            (* Determinant can only be computed for square matrices *)
            if row <> col then raise IncompatibleDimensions
            else
                if row = 1 then m.(0).(0)
                else if row = 2 then 
                    M.sub (M.mul m.(0).(0) m.(1).(1)) 
        		      	  (M.mul m.(0).(1) m.(1).(0))
                else 
                    let determinant = ref M.zero in
                    for i = 0 to row - 1 do
                        let next_mat = zero ~dimx:(row - 1) ~dimy:(row - 1) in
                        for j = 1 to row - 1 do
                            for k = 0 to row - 1 do
                                if k < i then next_mat.(j-1).(k) <- m.(j).(k)
                                else if k > i then next_mat.(j-1).(k-1) <- m.(j).(k)
                            done;
                        done;

              (* Sign of the determinant alternates with each row *)
              let sign = ref M.one in
              if (i mod 2 = 1) then sign := (M.sub M.zero M.one);

              determinant := M.add !determinant 
        			   (M.mul (M.mul !sign m.(0).(i)) (det next_mat));
            done;
            !determinant
        ;;
                  
        (* Computes the LU decomposition of a matrix such that PM = LU at the end *)
        let lu_decomp m =
            let row, col = dim m in
            (* LU_decomposition only works for square matrices *)
            if row <> col then raise IncompatibleDimensions
            else
                let lower = identity row in
                let pivot_mat = identity row in
                (* Create a copy of the matrix passed in to not alter the original values *)
                let upper = zero ~dimx:row ~dimy:row in
                for i = 0 to row - 1 do
                    upper.(i) <- Array.copy m.(i);
                done;

           
            (* Find the largest value in a row (the pivot row) for the kth column *)
            for k = 0 to row - 1 do
              let max = ref k in
                for i = k + 1 to col - 1 do
                    match M.comp (M.abs_val m.(i).(k)) (M.abs_val m.(!max).(k)) with
                    | Greater -> (max := i);
                    | _ -> ();
                done;

    	       (* Swap the pivot row with the top row we are working with *)
                swap_array_from_to upper.(k) upper.(!max) k (row - 1);
                swap_array_from_to lower.(k) lower.(!max) 0 (k - 1);
    	       let temp_row = pivot_mat.(k) in
                pivot_mat.(k) <- pivot_mat.(!max);
                pivot_mat.(!max) <- temp_row;
        
                for i = k + 1 to row - 1 do
                    lower.(i).(k) <- M.div upper.(i).(k) upper.(k).(k);
                    for j = k to row - 1 do
                        upper.(i).(j) <- 
    		              (M.sub upper.(i).(j) (M.mul lower.(i).(k) upper.(k).(j)));
                    done;
                done;     
            done;
            (upper, lower, pivot_mat)

        (* Solves a linear system when given a square matrix m and a solution vector b_original *)
        let solve m b_original =
            let length = Array.length b_original in
            let row, col = dim m in
           
            (* Algorithm works for invertible, square matrices - noninvertible 
               matrices might not yield a solution *)
            if length <> row || row <> col then raise IncompatibleDimensions
            else
            if M.abs_val (det m) <= M.epsilon then 
              raise (failwith "not an invertible matrix - might not have a solution")
            else
            
            (* Create a copy of the matrix passed in to not alter the original values *)
            let b = zero ~dimx:length ~dimy:1 in
            for i = 0 to length - 1 do
                    b.(i) <- Array.copy b_original.(i);
                done;
            (* Find the largest value in a row (the pivot row) for the kth column *)
            for k = 0 to col - 1 do
                let max = ref k in
                for i = k + 1 to row - 1 do
                    match M.comp (M.abs_val m.(i).(k)) (M.abs_val m.(!max).(k)) with
                    | Greater -> (max := i);
                    | _ -> ();
                done;
                
        	(* Swap the pivot row with the top row we are working with *)
                let temp_row = m.(k) in
                    m.(k) <- m.(!max);
                    m.(!max) <- temp_row;
                let temp_b = b.(k).(0) in
                    b.(k).(0) <- b.(!max).(0);
                    b.(!max).(0) <- temp_b;

        	(* Pivot between A and b - row-reduce without leading ones *)
                for i = k + 1 to length - 1 do
                    let factor = M.div m.(i).(k) m.(k).(k) in
                    b.(i).(0) <- M.sub b.(i).(0) (M.mul factor b.(k).(0));
                    for j = k to length - 1 do
                        m.(i).(j) <- M.sub m.(i).(j) (M.mul factor m.(k).(j));
                    done;
                done;    
            done; 

            (* Substitute back to yield the solution vector *)
            let solution = zero ~dimx:length ~dimy:1 in
            for i = col - 1 downto 0 do
                let sum = ref M.zero in
                for j = i + 1 to row - 1 do
                    sum := M.add !sum (M.mul m.(i).(j) solution.(j).(0));
                done;
                solution.(i).(0) <- M.div (M.sub b.(i).(0) !sum) m.(i).(i);
            done;
            (m, solution)

        (* Adds the child matrix to the parent matrix starting at index (row, col) *)
        let join parent child row col =  
            let row_child, col_child = dim child in
            for i = 0 to row_child - 1 do
                for j = 0 to col_child - 1 do
                    parent.(i + row).(j + col) <- child.(i).(j) 
                done;
             done; parent;;

        (* Checks if dimensions are both 1 (base case) or odd dimensions and makes even *)
        let pad m1 = 
            let row, col = dim m1 in
            if row = col && row = 1 then m1
            else 
            (match row mod 2 = 0, col mod 2 = 0 with
            | true, true -> m1
            | true, false -> join (zero ~dimx:row ~dimy:(col + 1)) m1 0 0 
            | false, true -> join (zero ~dimx:(row + 1) ~dimy:col) m1 0 0
            | _, _ ->  join (zero ~dimx:(row + 1) ~dimy:(col + 1)) m1 0 0);;

        (* Adds the parent matrix to the child matrix starting at index (row, col) till child is full *)
        let split parent child row col =
            let row_child, col_child = dim child in 
            for i = 0 to row_child - 1 do
                for j = 0 to col_child - 1 do
                    child.(i).(j) <- parent.(i + row).(j + col)
                done;
             done; child;;

        (* Follows the invariant that the matrix has an even number of rows & columns *)
        let rec mul_invariant matrix1 matrix2 =
            (* Saves rows & columns of matrices for future use*)
            let row1, col1 = dim matrix1 in
            let row2, col2 = dim matrix2 in
            let result = zero ~dimx:row1 ~dimy:row1 in
            if row1 = 1 then 
                (result.(0).(0) <- M.mul matrix1.(0).(0) matrix2.(0).(0); result)
            else
                let half_row1 = row1 / 2 in
                let half_col1 = col1 / 2 in
                let half_row2 = row2 / 2 in
                let half_col2 = col2 / 2 in

                (* Create halves *)

                let a11 = zero ~dimx:half_row1 ~dimy:half_col1 in
                let a12 = zero ~dimx:half_row1 ~dimy:half_col1 in
                let a21 = zero ~dimx:half_row1 ~dimy:half_col1 in
                let a22 = zero ~dimx:half_row1 ~dimy:half_col1 in
                let b11 = zero ~dimx:half_row2 ~dimy:half_col2 in
                let b12 = zero ~dimx:half_row2 ~dimy:half_col2 in
                let b21 = zero ~dimx:half_row2 ~dimy:half_col2 in
                let b22 = zero ~dimx:half_row2 ~dimy:half_col2 in

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
            if dim m1 = dim m1_padded && dim m2 = dim m2_padded then result_padded
            else let result = zero ~dimx:(Array.length m1) ~dimy:(Array.length m2.(0)) in
            let _ = split result_padded result 0 0 in
            result;;

        (* Check if multiplication can be done *)
        let mul m1 m2 =
            if Array.length m1.(0) = Array.length m2 then mul_pad m1 m2
            else raise IncompatibleDimensions

    end




