open Matrix
open Ring

module MatrixFunctor (M : RING) : MATRIX with type elt = M.t =
	struct
		open Strassen
		open LU 
		open GaussianElimination

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
	    	do_operation m1 m2 M.add

	    let sub m1 m2 = 
	    	do_operation m1 m2 M.sub 
	    	
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