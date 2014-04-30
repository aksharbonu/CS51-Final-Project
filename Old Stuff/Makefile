all: final_project

FILES=GaussianElimination.ml Matrix.ml Strassen.ml LU.ml MatrixFunctor.ml Math.ml Akshar-Testing.ml

final_project: $(FILES)
	@echo "Compiling..."
	corebuild GaussianElimination.native
	corebuild Matrix.native
	corebuild Strassen.native
	corebuild LU.native
	corebuild MatrixFunctor.native
	corebuild Math.native
	corebuild Akshar-Testing.ml

check: $(FILES)
	chmod u+x ../check_width
	../check_width GaussianElimination.ml
	../check_width Matrix.ml
	../check_width Strassen.ml
	../check_width LU.ml
	../check_width MarixFunctor.ml
	../check_width Math.ml
	../check_width Akshar-Testing.ml

clean:
	rm -rf _build *.native