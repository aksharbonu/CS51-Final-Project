Open Matrix
Open Math

module MatrixFunctor (M : MATH) : MATRIX with type elt = M.t =
	struct
		Open Extras
		Open Strassen
		Open LU 
		Open GaussianElimination	
	end