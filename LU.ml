let LU_decomp (m : MATRIX) =
	let rows = Array.length m in
	let cols = Array.length m.(0) in
	let lower = zero rows cols in
	let upper = zero rows cols in
	for k = 0 to rows - 1 do
		lower.(k).(k) = 1
		for i = k + 1 to cols - 1 do
			lower.(i).(k) <- M.div m.(i).(k) m.(k).(k) 
			for j = k + 1 to cols - 1 do
				m.(i).(j) <- M.sub m.(i).(j) (M.mul lower.(i).(k) * m.(k).(j)) 
			done;
		done;
		for j = k to cols - 1 do
			upper.(k).(j) <- m.(k).(j)
		done;
	done;
	(upper, lower)