open Core.Std

type comparison = Less | Equal | Greater;;

module type MATRIX =
sig
    exception IncompatibleDimensions
    type elt
    type t
    val of_array: elt array array -> t
    val to_array: t -> elt array array
    val zero : int -> int -> t
    val identity : int -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val scalar : elt -> t -> t
    val det : t -> elt
    val mul : t -> t -> t
    val solve : t -> elt array -> t * elt array
    val compare : t -> t -> comparison
    val lu_decomp : t -> t * t * t
end

module type RING =
    sig
        type t
        val zero : t
        val one : t
        val epsilon : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val comp : 'a -> 'a -> comparison
        val abs_val : t -> t
        val print_elt : t -> unit
    end

module IntRing  = 
    struct 
        type t = int
        let zero = 0
        let epsilon = 0.0001
        let one = 1
        let add = (+)
        let sub = (-)
        let mul = ( * )         
        let div = (/)  
        let comp v1 v2 = if v1 = v2 then Equal else if v1 < v2 then Less
			 else Greater
        let abs_val v = abs v
        let print_elt s = print_endline (Int.to_string s)
    end

module FloatRing =
    struct
        type t = float
        let zero = 0.
        let one = 1.
        (* Small value for comparisons - cannot use = *)
        let epsilon = 0.0001
        let add = (+.)
        let sub = (-.)
        let mul = ( *. )
        let div = (/.)
        let comp v1 v2 = if v1 = v2 then Equal else if v1 < v2 then Less
			 else Greater
        let abs_val v = abs_float v
        let print_elt s = print_endline (Float.to_string s)
    end

module MatrixFunctor (M : RING) : MATRIX with type elt = M.t =
    struct

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
                for i = 0 to Array.length m1 - 1 do
                    for j = 0 to Array.length m1.(0) - 1 do
                        result.(i).(j) <- M.mul m1.(i).(j) value
                    done;
                done;
            result;;

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

        let compare v1 v2 =
            if v1 = v2 then Equal else if v1 < v2 then Less else Greater

        (* A method that swaps values in two arrays from one index to another *)
        let swap_array_from_to a1 a2 first last =
            for i = first to last do
                let temp_val = a1.(i) in
                a1.(i) <- a2.(i);
                a2.(i) <- temp_val;
            done;
        ;;

        let rec det m =
            let dim = Array.length m in
            let cols = Array.length m.(0) in
            (* Determinant can only be computed for square matrices *)
            if dim <> cols then raise IncompatibleDimensions
            else
                if dim = 1 then m.(0).(0)
                else if dim = 2 then 
                    M.sub (M.mul m.(0).(0) m.(1).(1)) 
        		      	  (M.mul m.(0).(1) m.(1).(0))
                else 
                    let determinant = ref M.zero in
                    for i = 0 to dim - 1 do
                        let next_mat = zero (dim - 1) (dim - 1) in
                        for j = 1 to dim - 1 do
                            for k = 0 to dim - 1 do
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
                  

        let lu_decomp m =
            let rows = Array.length m in
            let cols = Array.length m.(0) in
            (* LU_decomposition only works for square matrices *)
            if rows <> cols then raise IncompatibleDimensions
            else
                let lower = identity rows in
                let upper = m in
                let pivot_mat = identity rows in
           
            (* Find the largest value in a row (the pivot row) for the kth column *)
            for k = 0 to rows - 1 do
              let max = ref k in
                for i = k + 1 to cols - 1 do
                    match M.comp (M.abs_val m.(i).(k)) (M.abs_val m.(!max).(k)) with
                    | Greater -> (max := i);
                    | _ -> ();
                done;

    	       (* Swap the pivot row with the top row we are working with *)
                swap_array_from_to upper.(k) upper.(!max) k (rows - 1);
                swap_array_from_to lower.(k) lower.(!max) 0 (k - 1);
    	       let temp_row = pivot_mat.(k) in
                pivot_mat.(k) <- pivot_mat.(!max);
                pivot_mat.(!max) <- temp_row;
        
                for i = k + 1 to rows - 1 do
                    lower.(i).(k) <- M.div upper.(i).(k) upper.(k).(k);
                    for j = k to rows - 1 do
                        upper.(i).(j) <- 
    		              (M.sub upper.(i).(j) (M.mul lower.(i).(k) upper.(k).(j)));
                    done;
                done;     
            done;
            (upper, lower, pivot_mat)

        let solve m b =
            let length = Array.length b in
            let rows = Array.length m in
            let cols = Array.length m.(0) in
           
            (* Algorithm works for invertible, square matrices - noninvertible 
               matrices might not yield a solution *)
            if length <> rows || rows <> cols then raise IncompatibleDimensions
            else
            if M.abs_val (det m) < M.epsilon then 
              raise (failwith "not an invertible matrix - might not have a solution")
            else
            (* Find the largest value in a row (the pivot row) for the kth column *)
            for k = 0 to cols - 1 do
                let max = ref k in
                for i = k + 1 to rows - 1 do
                    match M.comp (M.abs_val m.(i).(k)) (M.abs_val m.(!max).(k)) with
                    | Greater -> (max := i);
                    | _ -> ();
                done;

        	(* Swap the pivot row with the top row we are working with *)
                let temp_row = m.(k) in
                    m.(k) <- m.(!max);
                    m.(!max) <- temp_row;
                let temp_b = b.(k) in
                    b.(k) <- b.(!max);
                    b.(!max) <- temp_b;

        	(* Pivot between A and b - row-reduce without leading ones *)
                for i = k + 1 to length - 1 do
                    let factor = M.div m.(i).(k) m.(k).(k) in
                    b.(i) <- M.sub b.(i) (M.mul factor b.(k));
                    for j = k to length - 1 do
                        m.(i).(j) <- M.sub m.(i).(j) (M.mul factor m.(k).(j));
                    done;
                done;    
            done; 

            (* Substitute back to yield the solution vector *)
            let solution = Array.create cols M.zero in
            for i = cols - 1 downto 0 do
                let sum = ref M.zero in
                for j = i + 1 to rows - 1 do
                    sum := M.add !sum (M.mul m.(i).(j) solution.(j));
                done;
                solution.(i) <- M.div (M.sub b.(i) !sum) m.(i).(i);
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
            | true, false -> join (zero row (col + 1)) m1 0 0 
            | false, true -> join (zero (row + 1) col) m1 0 0
            | _, _ ->  join (zero (row + 1) (col + 1)) m1 0 0);;

        (* Adds the parent matrix to the child matrix starting at index (row, col) till child is full *)
        let split parent child row col =
            let row_child, col_child = dim child in 
            for i = 0 to row_child - 1 do
                for j = 0 to col_child - 1 do
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
            if dim m1 = dim m1_padded && dim m2 = dim m2_padded then result_padded
            else let result = zero (Array.length m1) (Array.length m2.(0)) in
            let _ = split result_padded result 0 0 in
            result;;

        (* Check if multiplication can be done *)
        let mul m1 m2 =
            if Array.length m1.(0) = Array.length m2 then mul_pad m1 m2
            else raise IncompatibleDimensions

    end

(* LU & GAUSSIAN TESTING *)

module FloatMatrix = MatrixFunctor (FloatRing);;
let matrix1 = [|[|4.; 3.|]; [|6.; 3.|]|];;
assert (FloatMatrix.det (FloatMatrix.of_array matrix1) = -6.);;
let matrix2 = [|[|8.; 2.; 9.|]; [|4.; 9.; 4.|]; [|6.; 7.; 9.|]|];;
let matrix3 = [|[|1.; 0.|]; [|0.; 1.|]|];;
assert (FloatMatrix.det (FloatMatrix.of_array matrix3) = 1.);;
let (u1, l1, p1) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix1);;
let (u2, l2, p2) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix2);;
let (u3, l3, p3) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix3);;
assert (FloatMatrix.to_array u1 = [|[|6.; 3.|]; [|0.; 1.|]|]);;
assert (FloatMatrix.to_array l1 = [|[|1.; 0.|]; [|(2./.3.); 1.|]|]);;
assert (FloatMatrix.to_array u2 =
	  [|[|8.; 2.; 9.|]; [|0.; 8.; -0.5|]; [|0.; 0.; 2.59375|]|]);;
assert (FloatMatrix.to_array l2 = 
	  [|[|1.; 0.; 0.|]; [|0.5; 1.; 0.|]; [|0.75; 0.6875; 1.|]|]);;
assert (FloatMatrix.to_array u3 = matrix3);;
assert (FloatMatrix.to_array l3 = matrix3);;
(* Need Akshar's Multiplication function in order to test the two below
assert (FloatMatrix.to_array (FloatMatrix.mul u1 l1) = matrix1);;
assert (FloatMatrix.to_array (FloatMatrix.mul u2 l2) = matrix2);;
 *)
let (m1, sol1) = FloatMatrix.solve (FloatMatrix.of_array matrix3) (Array.create 2 1.);;
assert (sol1 = [|1.; 1.|]);;

(* Should work - need to fix determinant calculation, matrix is invertible *)
let matrix4 = [|[|0.; 1.; 1.|]; [|2.; 4.; -2.;|]; [|0.; 3.; 15.|]|];;
let (m4, sol4) = FloatMatrix.solve
		   (FloatMatrix.of_array matrix4) ([|4.; 2.; 36.|]);;
assert (sol4 = [|-1.; 2.; 2.|]);;

let matrix5 = [|[|10.; -7.; 0.|]; [|-3.; 2.; 6.|]; [|5.; -1.; 5.|]|];;
let (u5, l5, p5) = FloatMatrix.lu_decomp (FloatMatrix.of_array matrix5);;
assert (FloatMatrix.to_array u5 =
	  [|[|10.; -7.; 0.|]; [|0.; 2.5; 5.|]; [|0.; 0.; 6.2|]|]);;
assert (FloatMatrix.to_array l5 = 
	  [|[|1.; 0.; 0.|]; [|0.5; 1.; 0.|]; [|-0.3; -0.04; 1.|]|]);;
assert (FloatMatrix.to_array p5 = 
	  [|[|1.; 0.; 0.|]; [|0.; 0.; 1.|]; [|0.; 1.; 0.|]|]);;

(* STRASSEN TESTING *)

(* Test FloatRing *)

module FloatMatrix = MatrixFunctor (FloatRing);;
let matrix1f = FloatMatrix.of_array (Array.make_matrix 5 3 2.);;
let matrix2f = FloatMatrix.of_array (Array.make_matrix 3 5 3.);;
FloatMatrix.to_array (FloatMatrix.mul matrix1f matrix2f);;

let matrix3f = FloatMatrix.of_array 
[|
[|1.; 2.; 3.; 4.|];
[|5.; 6.; 7.; 8.|]; 
[|-1.; -2.; -3.; -4.|];
[|-5.; -6.; -7.; -8.|]; 
[|1.; 2.; 3.; 4.|];
|];;

let matrix4f = FloatMatrix.of_array 
[|
[|1.; 2.; 3.; 4.; 5.|];
[|5.; 6.; 7.; 8.; 9.|]; 
[|-1.; -2.; -3.; -4.; -5.|];
[|-5.; -6.; -7.; -8.; -9.|]; 
|];;

FloatMatrix.to_array (FloatMatrix.mul matrix3f matrix4f);;

(* Test IntRing *)

module IntMatrix = MatrixFunctor (IntRing);;
let matrix1i = IntMatrix.of_array (Array.make_matrix 5 3 2);;
let matrix2i = IntMatrix.of_array (Array.make_matrix 3 5 3);;
IntMatrix.to_array (IntMatrix.mul matrix1i matrix2i);;

let matrix3i = IntMatrix.of_array 
[|
[|1; 2; 3; 4|];
[|5; 6; 7; 8|]; 
[|-1; -2; -3; -4|];
[|-5; -6; -7; -8|]; 
[|1; 2; 3; 4|];
|];;

let matrix4i = IntMatrix.of_array 
[|
[|1; 2; 3; 4; 5|];
[|5; 6; 7; 8; 9|]; 
[|-1; -2; -3; -4; -5|];
[|-5; -6; -7; -8; -9|]; 
|];;

IntMatrix.to_array (IntMatrix.mul matrix3i matrix4i);;


