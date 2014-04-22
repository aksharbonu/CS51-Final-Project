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
    val mul : t -> t -> t
    (* val det : t -> int
    val inverse : t -> t
    val solve : t -> t -> t *)
end

module type RING =
    sig
        type t
        val zero : t
        val one : t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
    end

module IntRing = 
    struct 
        type t = int
        let zero = 0
        let one = 1
        let add = (+)
        let sub = (-)
        let mul = ( * )         
        let div = (/)   
    end;;

module FloatRing =
    struct
        type t = float
        let zero = 0.
        let one = 1.
        let add = (+.)
        let sub = (-.)
        let mul = ( *. )
        let div = (/.)
    end

module MatrixFunctor (M : RING) : MATRIX with type elt = M.t =
	struct
		(* 
			open LU 
			open GaussianElimination	
		*)

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

        let m1 = mul_invariant (add a11 a22) (add b11 b22) in 
        let m2 = mul_invariant (add a21 a22) b11 in 
        let m3 = mul_invariant a11 (sub b12 b22) in
        let m4 = mul_invariant a22 (sub b21 b11) in
        let m5 = mul_invariant (add a11 a12) b22 in
        let m6 = mul_invariant (sub a21 a11) (add b11 b12) in
        let m7 = mul_invariant (sub a12 a22) (add b21 b22) in

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

        result;;

let mul m1 m2 =
    mul_invariant m1 m2;; 

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