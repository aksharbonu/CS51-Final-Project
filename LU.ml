let LU_decomp (m : MATRIX) =
	let rows = Array.length m in
	let cols = Array.length m.(0) in
	let lower = Array.copy m in
	let upper = Array.copy m in
	for k = 0 to rows - 1 do
		lower.(k).(k) = 1
		for i = k + 1 to cols - 1 do
			lower.(i).(k) = (Float.of_int m.(i).(k))/m.(k).(k) (* How are we handling different types here with division? *)
			for j = k + 1 to cols - 1 do
				m.(i).(j) = m.(i).(j) - lower.(i).(k) * m.(k).(j) (* How are we passing in the type of the element so that we know what operations to perform? *)
			done;
		done;
		for j = k to cols - 1 do
			upper.(k).(j) = m.(k).(j)
		done;
	done;
	(upper, lower)