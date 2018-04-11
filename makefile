all: points masses

points: points.ml 
	ocamlbuild points.byte

masses: masses.ml 
	ocamlbuild masses.byte
