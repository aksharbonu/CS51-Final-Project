let LU_decomp (m : MATRIX) =
	let (upper, lower) = create_triangular m in
	(upper, lower)
	(* TODO - the actual LU decomposition algorithm *)

let create_triangular (m : Matrix) in
	let row = Array.length m in
	let col = Array.length m.(0) in 
	let upper = Array.copy m in
	    for i = 0 to row - 1 do
	   		for j = 0 to col - 1 do
	   			if i >= j then upper.(i).(j) <- 0
	   		done;
	   	done;
	let lower = Array.copy m in
		for i = 0 to row - 1 do
	   		for j = 0 to col - 1 do
	   			if i <= j then lower.(i).(j) <- 0
	   		done;
	   	done;
	(lower, upper)