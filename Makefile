all: final_project

FILES=ring.ml test.ml matrix.ml 

final_project: $(FILES)
	@echo "Compiling..."
	corebuild ring.native
	corebuild test.native
	corebuild matrix.native

check: $(FILES)
	chmod u+x ../check_width
	../check_width ring.ml
	../check_width test.ml
	../check_width matrix.ml

clean:
	rm -rf _build *.native