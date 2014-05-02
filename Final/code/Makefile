all: final_project

FILES=ring.ml test.ml matrix.ml 

final_project: $(FILES)
	@echo "Compiling..."
	corebuild ring.native
	corebuild test.native
	corebuild matrix.native

clean:
	rm -rf _build *.native